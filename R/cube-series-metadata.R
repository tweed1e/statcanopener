
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
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getCubeMetadata(35100003)
#' }
getCubeMetadata <- function(product_id) {
  # getCubeMetadata
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata
  #
  # POST BODY:
  #   [{"productId":35100003}]


  if (!is.character(product_id) & !is.integer(product_id)) {
    stop(paste0("Product ID must be a character or integer vector"), call. = FALSE)
  } # return actual class in message

  if (!grepl("^[0-9]+$", product_id)) {
    stop(paste0("Product ID must be an integer >= 0"), call. = FALSE)
  } # return actual class in message

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
#' @return A json object.
#'
#' @examples
#' \donttest{
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
#' @return A json object.
#'
#' @examples
#' \donttest{
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
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getSeriesInfoFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
#' }
#'
getSeriesInfoFromCubePidCoord <- function(product_id, coordinate) {

  if (!is.character(product_id) & !is.integer(product_id)) {
    stop(paste0("Product ID must be a character or integer vector"), call. = FALSE)
  } # return actual class in message

  if (!grepl("^[0-9]+$", product_id)) {
    stop(paste0("Product ID must be an integer >= 0"), call. = FALSE)
  } # return actual class in message

  if (!is.character(coordinate)) {
    stop(paste0("Cooedinate must be a string"), call. = FALSE)
  }

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
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getSeriesInfoFromVector(32164132)
#' }
#'
getSeriesInfoFromVector <- function(vector_id) {

  if (!is.character(vector_id) & !is.integer(vector_id)) {
    stop(paste0("Vector must be a character or integer vector."), call. = FALSE)
  } # return actual class in message

  if (!grepl("^v?[0-9]+$", vector_id)) {
    stop(paste0("Vector ID must be an integer >= 0"), call. = FALSE)
  } # return actual class in message

  vector_id <- sub("^v", "", vector_id, ignore.case = TRUE) # converts to character

  body <- paste0('[{"vectorId":', vector_id, ']}')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector",
    body = body,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}





