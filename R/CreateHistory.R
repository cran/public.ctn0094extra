#' Create a Subject History Table
#'
#' @param rawData_ls a list of tibbles returned by \code{\link{loadRawData}}
#' @param personsTable What is the name of the data table that contains all the
#'    subject IDs? Defaults to \code{"everybody"}. If no such table exists, then
#'    set this value to \code{"none"} and the unique subject IDs will be drawn
#'    from all tables in \code{rawData_ls}
#' @param firstDay_int (OPTIONAL) What should be the first "day" counter for all
#'    subjects? This may be beneficial to set if you wish to have the History
#'    ignore all days before a certain point.
#' @param lastDay_int (OPTIONAL) What should be the last "day" counter for all
#'    subjects?
#'
#' @return A tibble with columns \code{who} and \code{when}. Each subject will
#'    have one row for each day in the study range.
#'
#' @details We may want to perform SQL-like operations on a set of tables. This
#'    data table will form the "backbone" for future join operations. It creates
#'    one record per person for each day in the study. The default behavior is
#'    to set the date range to the smallest observed value for "when" across all
#'    data tables to the largest observed value for "when" across all tables,
#'    inclusive. Necessarily, this is \emph{very} sensitive to outliers or
#'    coding errors (for instance, if one subject has a "first day" 200 days
#'    before Study Day 0, then this History table will include days -200 to -1
#'    for ALL subjects, regardless of any recorded contact in this period).
#'
#'    If this behavior is not desirable, you must specify a study range. For
#'    example, you may have a short recruitment period followed by a 6-month
#'    clinical trial. In this instance, you may want to ignore any data more
#'    than 30 days prior to Study Day 0 for each subject or more than a few
#'    weeks after the end of the trial. Thus, you would set
#'    \code{firstDay_int = -30L} and \code{lastDay_int = 6 * 30 + 14}.
#'
#' @importFrom magrittr `%>%`
#' @importFrom purrr map_if map
#' @importFrom tibble tibble
#' @importFrom dplyr full_join
#' @export
#'
#' @examples
#'    data_ls <- loadRawData(c("tlfb", "all_drugs", "everybody"))
#'    CreateHistory(rawData_ls = data_ls, firstDay_int = -30)
CreateHistory <- function(rawData_ls,
                          personsTable = "everybody",
                          firstDay_int = NULL, lastDay_int = NULL){
  # browser()

  ###  Create the Date Range  ###
  # TO DO: add a verbose option to print extra diagnostic messages. We probably
  #   want to warn the user if firstDay_int < -60? Maybe use a trim? Detect
  #   outliers?
  if(is.null(firstDay_int)) {
    firstDay_int <-
      map_if(
        .x = rawData_ls,
        .p = ~{"when" %in% colnames(.x)},
        .f = ~{min(.x$when, na.rm = TRUE)},
        .else = ~{Inf}
      ) %>%
      unlist() %>%
      min()
  }

  if(is.null(lastDay_int)) {
    lastDay_int <-
      map_if(
        .x = rawData_ls,
        .p = ~{"when" %in% colnames(.x)},
        .f = ~{max(.x$when, na.rm = TRUE)},
        .else = ~{-Inf}
      ) %>%
      unlist() %>%
      max()
  }

  backbone_df <- tibble(
    when = seq.int(from = firstDay_int, to = lastDay_int, by = 1L)
  )


  ###  Return  ###
  if(personsTable == "everybody") {
    # The user left personTable as the default, so we need to confirm that this
    #   table is included in the data list

    if(!("everybody" %in% names(rawData_ls))){
      stop(
        "You have specified that personsTable = 'everybody'. However, the data
  list does not contain the 'everybody' data table. Either include the 'everybody'
  data table or set the argument personsTable = 'none'. This action will find all
  unique subjects across all supplied tables.",
        call. = FALSE
      )
    }

    out_df <- full_join(
      rawData_ls[["everybody"]]["who"],
      backbone_df,
      by = character()
    )

  } else if(personsTable %in% names(rawData_ls)) {
    # The user has supplied their own version of the "everybody" table, or they
    #   want to use a different set of subjects to create the backbone. For
    #   example, maybe we want to create the backbone from only the randomized
    #   subjects (instead of everyone)
    out_df <- full_join(
      rawData_ls[[personsTable]]["who"],
      backbone_df,
      by = character()
    )

  } else {
    # We are assuming that *ALL* the data tables in our database contain the
    #   "who" column
    allPeople_char <- map(
      .x = rawData_ls,
      .f = "who"
    ) %>%
      unlist() %>%
      unique()

    out_df <- full_join(
      tibble(who = allPeople_char),
      backbone_df,
      by = character()
    )

  }

  out_df

}
