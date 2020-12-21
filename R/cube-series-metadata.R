
#' Get cube/table metadata
#'
#' Get cube/table titles, product ID, CANSIM ID, release date, and more.
#'
#' @param product_id Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @export
#'
#' @return An httr response object
#'
#' @examples
#' \dontrun{
#' getCubeMetadata(35100003)
#' }
getCubeMetadata <- function(product_id) {

  check_product_id(product_id)

  body <- paste0('[{"productId":', product_id, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata",
    body = body,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}




#' Get list of all available data tables
#'
#' Users can query the output database to provide a complete inventory of
#' data tables available through this Statistics Canada API. This command
#' accesses a comprehensive list of details about each table including
#' information at the dimension level.
#'
#' @export
#'
#' @return An httr response object
#'
#' @examples
#' \dontrun{
#' getAllCubesList()
#' }
getAllCubesList <- function() {
  httr::GET(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList",
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}



#' Get list of all available data tables with less metadata
#'
#' Users can query the output database to provide a complete inventory of data
#' tables available through this Statistics Canada API. This command accesses a
#' list of details about each table.  Unlike getAllCubesList, this method does
#' not return dimension or footnote information.
#'
#' @export
#'
#' @return An httr response object
#'
#' @examples
#' \dontrun{
#' getAllCubesListLite()
#' }
getAllCubesListLite <- function() {
  httr::GET(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesListLite",
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}



#' Get series metadata
#'
#' Get series titles, product ID, CANSIM ID, release date, and more.
#'
#' @param product_id Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#' @param coordinate Coordinate is a concatenation of the member ID values for
#'  each dimension. One value per dimension. (i.e. "1.3.1.1.1.1.0.0.0.0" ) A
#'  table PID number combined with a coordinate will identify a unique time
#'  series of data points.
#'
#' @export
#'
#' @return An httr response object
#'
#' @examples
#' \dontrun{
#' getSeriesInfoFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
#' }
#'
getSeriesInfoFromCubePidCoord <- function(product_id, coordinate) {

  check_product_id(product_id)
  check_coordinate(coordinate)

  body <- paste0('[{"productId":', product_id, ',"coordinate":,"', coordinate, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromCubePidCoord",
    body = body,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}




#' Get series metadata from vector ID
#'
#' Get series titles, product ID, CANSIM ID, release date, and more.
#'
#' @param vector_id Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#'
#' @export
#'
#' @return An httr response object
#'
#' @examples
#' \dontrun{
#' getSeriesInfoFromVector(32164132)
#' }
#'
getSeriesInfoFromVector <- function(vector_id) {

  check_vector_id(vector_id)

  vector_id <- sub("^v", "", vector_id, ignore.case = TRUE) # converts to character

  body <- paste0('[{"vectorId":', vector_id, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector",
    body = body,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}





