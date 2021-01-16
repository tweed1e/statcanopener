#' Get changed series data
#'
#' Get changed series data given a product ID and coordinate
#'
#' @param product_id Product Identification number (PID) is a unique product
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
getChangedSeriesDataFromCubePidCoord <- function(product_id, coordinate) {
  check_vector_id(product_id)
  check_coordinate(coordinate)

  post(
    url_func = "getChangedSeriesDataFromCubePidCoord",
    productId = product_id,
    coordinate = coordinate
  )
}


#' Get changed series data from vector
#'
#' Get changed series data from vector
#'
#' @param vector_id Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getChangedSeriesDataFromVector("74804")
#' getChangedSeriesDataFromVector("v74804")
#' getChangedSeriesDataFromVector(74804)
#' }
getChangedSeriesDataFromVector <- function(vector_id) {
  check_vector_id(vector_id)
  post(url_func = "getChangedSeriesDataFromVector", vectorId = vector_id)
}


#' Get changed series data
#'
#' Get changed series data given a product ID and coordinate
#'
#' @param product_id Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#' @param coordinate Coordinate is a concatenation of the member ID values for
#' each dimension. One value per dimension. (i.e. "1.3.1.1.1.1.0.0.0.0" ) A
#' table PID number combined with a coordinate will identify a unique time
#' series of data points.
#' @param periods Number of periods to return, starting from the most recent
#' available data point.
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getDataFromCubePidCoordAndLatestNPeriods(35100003, "1.12.0.0.0.0.0.0.0.0", 10)
#' }
#'
getDataFromCubePidCoordAndLatestNPeriods <- function(...) {
  UseMethod("getDataFromCubePidCoordAndLatestNPeriods")
}

#' @export
getDataFromCubePidCoordAndLatestNPeriods.numeric <- function(productId, coordinate, latestN = 10) {
  check_product_id(productId)
  check_coordinate(coordinate)
  check_periods(latestN)

  post(
    url_func = "getDataFromCubePidCoordAndLatestNPeriods",
    productId = productId,
    coordinate = coordinate,
    latestN = latestN
  )
}

#' @export
getDataFromCubePidCoordAndLatestNPeriods.data.frame <- function(product_df) {

  if (!all(names(product_df) %in% c("productId", "coordinate", "latestN"))) {
    stop("product_df must have columns named productId, coordinate, and latestN")
  }

  check_product_id(product_df[["productId"]])
  check_coordinate(product_df[["coordinate"]])
  check_periods(product_df[["latestN"]])

  product_df <- product_df[c("productId", "coordinate", "latestN")]

  post(url_func = "getDataFromCubePidCoordAndLatestNPeriods", product_df)
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
getDataFromVectorsAndLatestNPeriods <- function(...) {
  UseMethod("getDataFromVectorsAndLatestNPeriods")
}

#' @export
getDataFromVectorsAndLatestNPeriods.numeric <- function(vectorId, latestN = 10) {

  post(
    url_func = "getDataFromVectorsAndLatestNPeriods",
    vectorId = vectorId,
    latestN = latestN
  )
}

#' @export
getDataFromVectorsAndLatestNPeriods.data.frame <- function(vector_df) {

  if (!all(names(vector_df) %in% c("vectorId", "latestN"))) {
    stop("vector_df must have columns named vectorId and latestN")
  }

  check_vector_id(vector_df[["vectorId"]])
  check_periods(vector_df[["latestN"]])

  vector_df <- vector_df[c("vectorId", "latestN")]

  post(url_func = "getDataFromVectorsAndLatestNPeriods", vector_df)
}

#' Download a vector
#'
#' Download a vector
#'
#' @param vector_ids Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#' @param start_release_date Release date of vector data. The returned data
#' will have been released on or after this date. The release date may lag
#' behind the most recent reference date of the data by months or years.
#' @param end_release_date Release date of vector data. The returned data
#' will have been released on or before this date. The release date may lag
#' behind the most recent reference date of the data by months or years.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getBulkVectorDataByRange(74804)
#' getBulkVectorDataByRange(c(74804, 1))
#' }
getBulkVectorDataByRange <- function(vector_ids) {
  check_vector_id(vector_ids)

  params <- list(
    vectorIds = as.list(as.character(vector_ids)),
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
#' @param product_id Product Identification number (PID) is a unique product
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
getFullTableDownloadCSV <- function(product_id, language = c("en", "fr")) {
  check_product_id(product_id)

  if (!language %in% c("en", "fr")) {
    stop(paste0("Language must be either 'en' or 'fr'."), call. = FALSE)
  }

  url_func <- paste0("getFullTableDownloadCSV/", product_id, "/", language)

  get(url_func)
}



#' Get full table download link in SDMX format
#'
#' A direct link to download the table in SDMX format. This function
#' does not actually download the table itself.
#'
#' @param product_id Product Identification number (PID) is a unique product
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
getFullTableDownloadSDMX <- function(product_id) {
  check_product_id(product_id)

  url_func <- paste0("getFullTableDownloadSDMX/", product_id)

  get(url_func)
}
