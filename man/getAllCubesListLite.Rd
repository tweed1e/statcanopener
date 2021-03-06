% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube-series-metadata.R
\name{getAllCubesListLite}
\alias{getAllCubesListLite}
\title{Get list of all available data tables with less metadata}
\usage{
getAllCubesListLite()
}
\value{
An httr response object
}
\description{
Users can query the output database to provide a complete inventory of data
tables available through this Statistics Canada API. This command accesses a
list of details about each table.  Unlike getAllCubesList, this method does
not return dimension or footnote information.
}
\examples{
\dontrun{
getAllCubesListLite()
}
}
