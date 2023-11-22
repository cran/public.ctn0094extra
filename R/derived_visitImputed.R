#' @title Imputed Patient Visit Data
#' 
#' @description Given a series of weekly clinic visits described per protocol,
#'    this data marks subjects as present or missing.
#'
#' @details This contains planned visits. Not all appointments were kept. We
#'    indicate if an appointment was kept on a certain day by marking the
#'    subject as `"Present"` on that day. If the subject goes more than 7 days
#'    without a clinic visit, we mark the subject as `"Missing"` on days that
#'    are multiples of 7 from the randomization day. For subjects without a
#'    randomization day, weekly visits after day of consent are marked as 
#'    `"Missing"` instead. This data set is a derived data set; the script used
#'    to create it is `"scripts/create_visitImputed_20210909.R"`.
#'    
#'    NOTE: because our window is a strict weekly window, a subject who shows
#'    up for their clinic visit one or more days late will still be marked as
#'    missing on the day they were supposed to appear. This means that some
#'    subjects will be marked as having missed their weekly clinic visit on one
#'    day, but be present in the clinic the next. 
#'
#' @docType data
#'
#' @usage data(derived_visitImputed)
#'
#' @format A tibble with `r scales::comma(nrow(derived_visitImputed))` rows and
#'    columns:
#'    \describe{
#'      \item{who}{Patient ID}
#'      \item{when}{Study day}
#'      \item{visitImputed}{
#'        Marked as `"Present"` if the subject visited the clinic on that day,
#'        or `"Missing"` if the subject did not visit the clinic on a day we
#'        would have expected them to (based on regular weekly visits). 
#'      }
#' }
"derived_visitImputed"
