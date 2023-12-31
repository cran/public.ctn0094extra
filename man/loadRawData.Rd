% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadRawData.R
\name{loadRawData}
\alias{loadRawData}
\title{Load Data Sets into a List}
\usage{
loadRawData(dataNames_char)
}
\arguments{
\item{dataNames_char}{Names of data sets to load}
}
\value{
Loads data sets specified into the current function environment for
further evaluation (unused) and then returns these data sets as a named
list
}
\description{
Load Data Sets into a List
}
\details{
We may want to perform SQL-like operations on a set of tables
without loading each table into R's Global Environment separately. This
function loads these data sets into a self-destructing environment and
then returns a named list of these data sets.
}
\examples{
   loadRawData(c("tlfb", "all_drugs"))
}
