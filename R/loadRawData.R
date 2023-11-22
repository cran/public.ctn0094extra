#' Load Data Sets into a List
#'
#' @param dataNames_char Names of data sets to load
#'
#' @return Loads data sets specified into the current function environment for
#'    further evaluation (unused) and then returns these data sets as a named
#'    list
#'
#' @details We may want to perform SQL-like operations on a set of tables
#'    without loading each table into R's Global Environment separately. This
#'    function loads these data sets into a self-destructing environment and
#'    then returns a named list of these data sets.
#'
#' @import public.ctn0094data
#' @importFrom magrittr `%>%`
#' @importFrom purrr map
#' @importFrom utils data
#' @export
#'
#' @examples
#'    loadRawData(c("tlfb", "all_drugs"))
loadRawData <- function(dataNames_char){
  # browser()

  dataNames_symb <-
    dataNames_char %>%
    map(as.name)

  here_env <- environment()
  dataHere <- function(...){
    data(..., envir = here_env)
  }

  do.call("dataHere", dataNames_symb)
  mget(dataNames_char)

}
