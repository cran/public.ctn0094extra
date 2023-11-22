#' Create a Subject History Table by Protocol
#'
#' @param start_vec a named integer vector with the number of days before
#'    subject consent when the subject history should start, per protocol
#' @param end_vec a named integer vector with the length of the study phase of
#'    interest, per protocol
#' @param persons_df Either the name of the data frame that contains all the
#'    subject IDs and their clinical trials (which defaults to
#'    \code{"everybody"}), or a data frame with this information. See "Details"
#'    for more information.
#'
#' @return A tibble with columns \code{who}, \code{project}, and \code{when}.
#'    Each subject will have one row for each day in the study range.
#'
#' @details We may want to perform SQL-like operations on a set of tables. This
#'    data table will form the "backbone" for future join operations. It creates
#'    one record per person in each study for each day in those studies (when
#'    \code{persons_df} is set to \code{"everybody"}), or it creates one record
#'    per person contained in the table \code{persons_df} for each day in those
#'    studies. The default behavior is to use the supplied \code{"everybody"}
#'    table for all consenting subjects in the CTN-0027, CTN-0030, and CTN-0051
#'    clinical trials. However, users may only care about a smaller subset of
#'    these patients, so a subset of the \code{"everybody"} data frame can be
#'    supplied to the \code{persons_df} argument if desired.
#'    
#'    NOTE: this function is only appropriate for trial with fixed start and
#'    end days (such as CTN-0027 or CTN-0051). For studies with variable-length
#'    (i.e., subject-specific) protocol lengths, please use 
#'    \code{CreateSubjectProtocolHistory()} instead.
#'
#' @importFrom utils data
#' @importFrom magrittr `%>%`
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter full_join
#' @export
#'
#' @examples
#'    start_int <- c(`27` = -30L, `51` = -30L)
#'    end_int   <- c(`27` = 168L, `51` = 168L)
#'    
#'    CreateProtocolHistory(
#'      start_vec = start_int, end_vec = end_int
#'    )
#'    
CreateProtocolHistory <- function(start_vec, end_vec, persons_df = "everybody"){
  # browser()
  
  ###  Check Inputs  ###
  # Studies
  studies_char <- names(start_vec)
  if (!identical(names(end_vec), studies_char)) {
    stop("start_vec and end_vec must have the same names", call. = FALSE)
  }
  
  # Table of Participants
  here_env <- environment()
  if (identical(persons_df, "everybody")) {
    
    everybody <- NULL
    data("everybody", package = "public.ctn0094data", envir = here_env)
    thePeople_df <- everybody
    
  } else {
    
    isDF <- "data.frame" %in% class(persons_df)
    if (!isDF) {
      stop("persons_df must be a data frame/tibble.", call. = FALSE)
    } else {
      
      hasRightCols <- all(
        c("who", "project") %in% colnames(persons_df)
      )
      
      if (!hasRightCols) {
        stop("persons_df must have columns 'who' and 'project'.", call. = FALSE)
      } else {
        thePeople_df <- persons_df
      }
      
    }
    
  }
  
  project <- NULL
  thePeople_df <- 
    thePeople_df %>% 
    filter(project %in% studies_char)
  
  
  ###  Create the Days Backbone  ###
  data_ls <- vector(mode = "list", length = length(studies_char))
  for (study in seq_along(studies_char)) {
    
    daysSequence_int <- seq.int(
      from = start_vec[[study]],
      to = end_vec[[study]],
      by = 1L
    )
    
    data_ls[[study]] <- tibble(
      project = studies_char[study],
      when    = daysSequence_int
    )
    
  }
  
  backbone_df <- bind_rows(data_ls)
    
  ###  Combine and Return  ###
  out_df <- full_join(
    thePeople_df,
    backbone_df,
    by = "project"
  )
  
  out_df

}
