% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-access.R
\name{getDataFromVectorsAndLatestNPeriods}
\alias{getDataFromVectorsAndLatestNPeriods}
\title{Get changed series data from vector}
\usage{
getDataFromVectorsAndLatestNPeriods(vectorId, latestN)
}
\arguments{
\item{vectorId}{Vector is a short identifier to refer to a time series of
data points. Unique variable length reference code, consisting of the
letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)}

\item{latestN}{Number of periods to return, starting from the most recent
available data point.}
}
\value{
An httr response object
}
\description{
Get changed series data from vector
}
\examples{
\dontrun{
getDataFromVectorsAndLatestNPeriods(74804, 5)
}
}
