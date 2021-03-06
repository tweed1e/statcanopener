
#' Get cube/table metadata
#'
#' Get cube/table titles, product ID, CANSIM ID, release date, and more.
#'
#' @param productId Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getCubeMetadata(35100003)
#' }
getCubeMetadata <- function(productId) {
  check_product_id(productId)

  params_df <- data.frame(productId = productId)

  post(url_func = "getCubeMetadata", params_df)
}


#' Get list of all available data tables
#'
#' Users can query the output database to provide a complete inventory of
#' data tables available through this Statistics Canada API. This command
#' accesses a comprehensive list of details about each table including
#' information at the dimension level.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getAllCubesList()
#' }
getAllCubesList <- function() {
  get("getAllCubesList")
}



#' Get list of all available data tables with less metadata
#'
#' Users can query the output database to provide a complete inventory of data
#' tables available through this Statistics Canada API. This command accesses a
#' list of details about each table.  Unlike getAllCubesList, this method does
#' not return dimension or footnote information.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getAllCubesListLite()
#' }
getAllCubesListLite <- function() {
  get("getAllCubesListLite")
}



#' Get series metadata
#'
#' Get series titles, product ID, CANSIM ID, release date, and more.
#'
#' @param productId Product Identification number (PID) is a unique product
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
#' @return An httr response object
#' @examples
#' \dontrun{
#' getSeriesInfoFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
#' }
#'
getSeriesInfoFromCubePidCoord <- function(productId, coordinate) {
  check_product_id(productId)
  check_coordinate(coordinate)

  params_df <- data.frame(productId = productId, coordinate = coordinate)

  post(url_func = "getSeriesInfoFromCubePidCoord", params_df)
}


#' Get series metadata from vector ID
#'
#' Get series titles, product ID, CANSIM ID, release date, and more.
#'
#' @param vectorId Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getSeriesInfoFromVector(32164132)
#' }
getSeriesInfoFromVector <- function(vectorId) {
  check_vector_id(vectorId)

  params_df <- data.frame(vectorId = vectorId)

  post(url_func = "getSeriesInfoFromVector", params_df)
}
