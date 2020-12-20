



# Data Access; data changes for today, over time and full table


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
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getChangedSeriesDataFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
#' }
#'
getChangedSeriesDataFromCubePidCoord <- function(product_id, coordinate) {
  # getChangedSeriesDataFromCubePidCoord
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromCubePidCoord
  #
  # POST BODY:
  #   [{"productId": 35100003, "coordinate": "1.12.0.0.0.0.0.0.0.0"}]

  body <- paste0('[{"productId:"', product_id, ',"coordinate:","', coordinate, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromCubePidCoord",
    body = body,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
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
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getChangedSeriesDataFromVector("74804")
#' getChangedSeriesDataFromVector("v74804")
#' getChangedSeriesDataFromVector(74804)
#' }
getChangedSeriesDataFromVector <- function(vector_id) {

  # getChangedSeriesDataFromVector
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromVector
  #
  # POST BODY:
  #   [{"vectorId":32164132}]
  #

  body <- paste0('[{"vectorId":', vector_id, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromVector",
    body = body,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}


# For those who are looking to display data going back N reporting periods from today there are the following set of endpoints (methods). Both methods will return the same results. Our example uses the last three (3) reference periods.
#

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
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getDataFromCubePidCoordAndLatestNPeriods(35100003, "1.12.0.0.0.0.0.0.0.0", 10)
#' }
#'
getDataFromCubePidCoordAndLatestNPeriods <- function(product_id, coordinate, periods) {
  # getDataFromCubePidCoordAndLatestNPeriods
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods
  #
  # POST BODY:
  #   [{"productId": 35100003, "coordinate": "1.12.0.0.0.0.0.0.0.0", "latestN":3}]
  #

  body <- paste0('[{"productId:"', product_id,
                 ',"coordinate:","', coordinate,
                 ',latestN:', periods ,'}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods",
    body = body,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}





#' Get changed series data from vector
#'
#' Get changed series data from vector
#'
#' @param vector_id Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#' @param periods Number of periods to return, starting from the most recent
#' available data point.
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getDataFromVectorsAndLatestNPeriods("74804", 5)
#' getDataFromVectorsAndLatestNPeriods("v74804", 5)
#' getDataFromVectorsAndLatestNPeriods(74804, 5)
#' }
getDataFromVectorsAndLatestNPeriods <- function(vector_id, periods) {

  # getDataFromVectorsAndLatestNPeriods
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods
  #
  # POST BODY:
  #   [{"vectorId":32164132, "latestN":3}]


  body <- paste0('[{"vectorId":', vector_id, ',"latestN:', periods, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods",
    body = body,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}




#' Download a vector
#'
#' Download a vector
#'
#' @param vector_ids Vector is a short identifier to refer to a time series of
#' data points. Unique variable length reference code, consisting of the
#' letter 'V', followed by up to 10 digits. (i.e. V1234567890, V1, etc.)
#' @param start_release_date
#' @param end_release_date
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getBulkVectorDataByRange("74804")
#' getBulkVectorDataByRange("v74804")
#' getBulkVectorDataByRange(74804)
#' }
getBulkVectorDataByRange <- function(vector_ids,
                                     start_release_date = "1901-01-01",
                                     end_release_date = as.Date(Sys.time())) {
  # getBulkVectorDataByRange
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange
  #
  # POST BODY:
  #
  #   {
  #     "vectorIds": ["74804","1"],
  #     "startDataPointReleaseDate": "2015-12-01T08:30",
  #     "endDataPointReleaseDate": "2018-03-31T19:00"
  #   }

  # check args
  # dates are strings or dates that are meaningful
  # release dates are annoying
  if (!is.character(vector_ids) & !is.integer(vector_ids)) {
    stop(paste0("Vectors must be a character or integer vector."), call. = FALSE)
  } # return actual class in message

  if (!all(grepl("^v?[0-9]+$", vector_ids))) {
    stop(paste0("Each element of vectors must be an integer >= 0"), call. = FALSE)
  } # return actual class in message

  vector_ids <- sub("^v", "", vector_ids, ignore.case = TRUE) # converts to character

  # etc
  vectors_args <- paste0('"vectorIds":[\"',
                         paste(vector_ids, collapse = '","'),
                         '"]')

  times_args <- paste0(
    '"startDataPointReleaseDate":"', stc_time(start_release_date),
    '","endDataPointReleaseDate":"', stc_time(end_release_date),
    '"'
  )

  body <- paste0('[', vectors_args, times_args, ']')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange",
    body = body,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}




#' Get full table download link in CSV format
#'
#' Get cube/table titles, product ID, CANSIM ID, release date, and more.
#'
#' @param product_id Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @param language 'en' for English or 'fr' for French
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getFullTableDownloadCSV(35100003, "fr")
#' }
getFullTableDownloadCSV <- function(product_id, language = c("en", "fr")) {
  # getFullTableDownloadCSV
  # GET URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/14100287/en
  #
  # RESULT:
  #
  #   {
  #     "status": "SUCCESS",
  #     "object": "https://www150.statcan.gc.ca/n1/tbl/csv/14100287-eng.zip"
  #   }
  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/", product_id, "/", language)

  httr::GET(
    url = url,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}



#' Get full table download link in SDMX format
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
#' getFullTableDownloadSDMX(35100003)
#' }
getFullTableDownloadSDMX <- function(product_id) {
  # getFullTableDownloadSDMX
  # GET URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadSDMX/14100287
  #
  # RESULT:
  #
  #   {
  #     "status": "SUCCESS",
  #     "object": "https://www150.statcan.gc.ca/n1/tbl/sdmx/14100287-SDMX.zip"
  #   }
  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadSDMX/", product_id)

  httr::GET(
    url = url,
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}




