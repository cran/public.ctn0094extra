#' Create a Subject History Table by Protocol
#'
#' @param randCTN30_df A data frame of subject randomization days for CTN-0030.
#'    This data will have columns for \code{who}, \code{which}, and \code{when}.
#' @param start_int When should the protocol timeline start? Defaults to 30 days
#'    before consent (allowing for self-reported drug use via Timeline Follow-
#'    Back data).
#' @param phase1Len_int When should Phase I end per protocol? Defaults to 98
#'    days (14 weeks).
#' @param phase2Len_int When should Phase II end per protocol? Defaults to 168
#'    days (24 weeks).
#'
#' @return A tibble with columns \code{who} and \code{when}. Each subject will
#'    have one row for each day in the study range.
#'
#' @details We may want to perform SQL-like operations on a set of tables. This
#'    data table will form the "backbone" for future join operations. It creates
#'    one record per person in a study for each day in that study, and it is
#'    designed to work with subject-specific two-arm protocols (where Phase I
#'    and Phase II treatments could start on different days for each subject).
#'    For subjects who consented to treatment but were never randomized, we use
#'    an "intent to treat" philosophy and assign them an empty protocol timeline
#'    starting on the day of consent (day 0) until \code{phase1Len_int}.
#'
#'    NOTE: We expect this function to be used specifically to create a
#'    potential visit backbone for CTN-0030. For studies with fixed protocol
#'    lengths, please use \code{CreateSubjectHistory()} instead.
#'
#' @importFrom magrittr `%>%`
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#'    # Subject A started Phase I on day 2 and was switched from treatment
#'    #   Phase I to Phase II on day 45. Subject B started Phase I on day 6 and
#'    #   was never switched to Phase II (either because they dropped out of the
#'    #   study or because the treatment given to them in Phase I worked).
#'    rand_df <- data.frame(
#'      who   = c("A", "A", "B", "B"),
#'      which = c(  1,   2,   1,   2),
#'      when  = c(  2,  45,   6,  NA)
#'    )
#'
#'    # Based on this example data above, we expect to see potential contact
#'    #   days per the protocol for subject A to range from -30 to 45 + 168.
#'    #   For subject B, we expect this range to be from -30 to 6 + 98
#'    CreateCTN30ProtocolHistory(rand_df)
#'
CreateCTN30ProtocolHistory <- function(randCTN30_df,
                                       start_int = -30,
                                       phase1Len_int = 98,
                                       phase2Len_int = 168){

  who <- NULL
  data_ls <- split(randCTN30_df, randCTN30_df$"who")

  res_ls <- map(
    .x = data_ls,
    .f = ~{


      ###  Phase 0  ###
      # If the subject was never randomised to Phase I, then we will set the
      #   first randomization day to 0 (the day of consent), and build out the
      #   protocol timeline (for Phase I only) from that day on.
      firstRand_num <- .x[1, "when", drop = TRUE]
      if ( is.na(firstRand_num) ) { firstRand_num <- 0 }

      secondRand_num <- .x[2, "when", drop = TRUE]
      secondRand_lgl <- !is.na(secondRand_num)


      ###  Phase I  ###
      # If the subject was never randomized to Phase II, then the end of Phase I
      #   should be 14 * 7 = 98 days after randomisation
      phase1End_int <- ifelse(
        test = !secondRand_lgl,
        yes  = firstRand_num + phase1Len_int,
        no   = secondRand_num - 1
      )
      phase1_int <- seq.int(
        from = start_int,
        to   = phase1End_int,
        by   = 1L
      )


      ###  Phase II  ###
      # If the subject was randomized to Phase II, then the end of Phase II
      #   should be 24 * 7 = 168 days after the start of Phase II.
      # Programming note: ifelse() can't return NULL
      if (!secondRand_lgl) {
        phase2_int <- NULL
      } else {
        phase2_int <- seq.int(
          from = secondRand_num,
          to   = secondRand_num + phase2Len_int,
          by   = 1L
        )
      }


      ###  Subject Backbone  ###
      days_int <- c(phase1_int, phase2_int)
      who_char <- .x[1, "who", drop = TRUE]
      tibble(
        who  = rep(who_char, length(days_int)),
        when = days_int
      )

    }
  )

  bind_rows(res_ls)

}
