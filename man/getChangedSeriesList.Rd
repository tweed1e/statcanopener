% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/product-change-listings.R
\name{getChangedSeriesList}
\alias{getChangedSeriesList}
\title{Get list of changed series}
\usage{
getChangedSeriesList()
}
\value{
An httr response object
}
\description{
Users can choose to ask for what series have changed today. This can be
invoked at any time of day and will reflect the list of series that have
been updated at 8:30am EST on a given release up until midnight that same day.
Users can also query what has changed at the table/cube level on a specific
day by adding an ISO date to the end of the URL. This date can be any
date from today into the past.
}
\examples{
\donttest{
getChangedSeriesList()
}
}
