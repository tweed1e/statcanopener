# Product Change Listings

#' Get list of changed series
#'
#' Users can choose to ask for what series have changed today. This can be
#' invoked at any time of day and will reflect the list of series that have
#' been updated at 8:30am EST on a given release up until midnight that same day.
#' Users can also query what has changed at the table/cube level on a specific
#' day by adding an ISO date to the end of the URL. This date can be any
#' date from today into the past.
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getChangedSeriesList()
#' }
getChangedSeriesList <- function() {
  httr::GET(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesList",
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}


#' Get list of changed cubes/tables
#'
#' Users can choose to ask for what series have changed today. This can be
#' invoked at any time of day and will reflect the list of series that have
#' been updated at 8:30am EST on a given release up until midnight that same day.
#' Users can also query what has changed at the table/cube level on a specific
#' day by adding an ISO date to the end of the URL. This date can be any
#' date from today into the past.
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getChangedCubeList()
#' }
getChangedCubeList <- function() {
  httr::GET(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedCubeList/2017-12-07",
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}