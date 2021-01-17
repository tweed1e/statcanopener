#' Get changed series data
#'
#' Get changed series data given a product ID and coordinate
#'
#' @param productId Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#' @param coordinate Coordinate is a concatenation of the member ID values for
#' each dimension. One value per dimension. (i.e. "1.3.1.1.1.1.0.0.0.0" ) A
#' table PID number combined with a coordinate will identify a unique time
#' series of data points.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getChangedSeriesDataFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
#' }
#'
getChangedSeriesDataFromCubePidCoord <- function(productId, coordinate) {
  check_product_id(productId)
  check_coordinate(coordinate)

  params_df <- data.frame(
    productId = productId,
    coordinate = coordinate
  )

  post(url_func = "getChangedSeriesDataFromCubePidCoord", params_df)
}


#' Get changed series data from vector
#'
#' Get changed series data from vector
#'
#' @param vectorId Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getChangedSeriesDataFromVector(74804)
#' }
getChangedSeriesDataFromVector <- function(vectorId) {
  check_vector_id(vectorId)

  params_df <- data.frame(vectorId = vectorId)

  post(url_func = "getChangedSeriesDataFromVector", params_df)
}


#' Get changed series data
#'
#' Get changed series data given a product ID and coordinate
#'
#' @param productId Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#' @param coordinate Coordinate is a concatenation of the member ID values for
#' each dimension. One value per dimension. (i.e. "1.3.1.1.1.1.0.0.0.0" ) A
#' table PID number combined with a coordinate will identify a unique time
#' series of data points.
#' @param latestN Number of periods to return, starting from the most recent
#' available data point.
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getDataFromCubePidCoordAndLatestNPeriods(35100003, "1.12.0.0.0.0.0.0.0.0", 10)
#' }
#'
getDataFromCubePidCoordAndLatestNPeriods <- function(productId, coordinate, latestN) {
  check_product_id(productId)
  check_coordinate(coordinate)
  check_periods(latestN)

  params_df <- data.frame(
    productId = productId,
    coordinate = coordinate,
    latestN = latestN
  )

  post(url_func = "getDataFromCubePidCoordAndLatestNPeriods", params_df)
}



#' Get changed series data from vector
#'
#' Get changed series data from vector
#'
#' @param vectorId Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#' @param latestN Number of periods to return, starting from the most recent
#' available data point.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getDataFromVectorsAndLatestNPeriods(74804, 5)
#' }
getDataFromVectorsAndLatestNPeriods <- function(vectorId, latestN) {
  check_vector_id(vectorId)
  check_periods(latestN)

  params_df <- data.frame(vectorId = vectorId, latestN = latestN)

  post(url_func = "getDataFromVectorsAndLatestNPeriods", params_df)
}


#' Download a vector
#'
#' Download a vector
#'
#' @param vectorIds Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of up to 10
#' digits. (i.e. 1234567890, 1, etc.)
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getBulkVectorDataByRange(74804)
#' getBulkVectorDataByRange(c(74804, 1))
#' }
getBulkVectorDataByRange <- function(vectorIds) {
  check_vector_id(vectorIds)

  params <- list(
    vectorIds = as.list(as.character(vectorIds)),
    startDataPointReleaseDate = stc_time("1901-01-01"),
    endDataPointReleaseDate = stc_time(as.Date(Sys.time()))
  )

  post(url_func = "getBulkVectorDataByRange", params)
}


#' Get full table download link in CSV format
#'
#' A direct link to download the table in CSV format. This function
#' does not actually download the table itself.
#'
#' @param productId Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @param language 'en' for English or 'fr' for French
#' @export
#' @return A direct link to download the table in CSV format
#' @examples
#' \dontrun{
#' getFullTableDownloadCSV(35100003, "fr")
#' }
getFullTableDownloadCSV <- function(productId, language = c("en", "fr")) {
  check_product_id(productId)

  if (!language %in% c("en", "fr")) {
    stop(paste0("Language must be either 'en' or 'fr'."), call. = FALSE)
  }

  url_func <- paste0("getFullTableDownloadCSV/", productId, "/", language)

  get(url_func)
}



#' Get full table download link in SDMX format
#'
#' A direct link to download the table in SDMX format. This function
#' does not actually download the table itself.
#'
#' @param productId Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @export
#' @return A direct link to download the table in SDMX format
#' @examples
#' \dontrun{
#' getFullTableDownloadSDMX(35100003)
#' }
getFullTableDownloadSDMX <- function(productId) {
  check_product_id(productId)

  url_func <- paste0("getFullTableDownloadSDMX/", productId)

  get(url_func)
}
