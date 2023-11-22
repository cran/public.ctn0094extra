#' @title Derived Patient Race and Ethnicity Data
#'
#' @description Summarize the patients' self-reported race and ethnicity into
#'    four groups: "Non-Hispanic White", "Non-Hispanic Black", "Hispanic", and
#'    "Other".
#'
#' @details This data set contains a summary of self-reported race and ethnicity
#'    in four levels. Of note, the "Other" category includes 2 participants who
#'    marked "no" to the question of Hispanic/Latino ethnicity but refused to
#'    answer their race. Also, the "Other" category includes 33 participants for
#'    whom all information about race and ethnicity is missing. This data set is
#'    a derived data set; the script used to create it is
#'    `"scripts/create_raceEthnicity_20220816.R"`.
#'
#' @docType data
#'
#' @usage data(derived_raceEthnicity)
#'
#' @format A tibble with `r scales::comma(nrow(derived_raceEthnicity))`
#'    rows and columns:
#'    \describe{
#'      \item{who}{Patient ID}
#'      \item{race}{Self-reported race. Options are "White", "Black", "Other",
#'        and "Refused/missing".}
#'      \item{is_hispanic}{Self-reported Hispanic/Latino ethnicity.}
#'      \item{race_ethnicity}{Derived composite marker of race and ethnicity.}
#' }
"derived_raceEthnicity"
