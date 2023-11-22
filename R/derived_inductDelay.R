#' Derived Induction Delay Data
#'
#' @description This data set measures the number of days from a participant's
#'    randomization until they received their first dose of study drug.
#'
#' @details This data set is a derived data set. The inputs are the treatment
#'    and randomization data sets. The code to calculate the induction delay is
#'    given in `scripts/create_inductDelay_20210729.R`. The treatment arm is
#'    also included in this data set because the induction delay depends on the
#'    type of treatment. For example, inpatient treatment arms may have
#'    different protocols than outpatient treatment arms for determining how
#'    many days the subject must wait after randomization before treatment.
#'
#' @docType data
#'
#' @usage data(derived_inductDelay)
#'
#' @format A tibble with `r scales::comma(nrow(derived_inductDelay))` rows and `r scales::comma(ncol(derived_inductDelay))` variables:
#' \describe{
#'   \item{who}{Patient ID}
#'   \item{treatment}{What treatment is prescribed: "Inpatient BUP",
#'     "Inpatient NR-NTX", "Methadone", "Outpatient BUP", "Outpatient BUP + EMM",
#'     "Outpatient BUP + SMM"}
#'   \item{inductDelay}{How many days after being assigned to a treatment arm
#'      did the participant receive their first dose of study drug? Missing
#'      values indicate that the subject never received their first dose.}
#'   }
"derived_inductDelay"
