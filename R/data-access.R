



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
#' \dontrun{
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

  check_product_id(product_id)
  check_coordinate(coordinate)

  body <- paste0('[{"productId":', product_id, ',"coordinate":,"', coordinate, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromCubePidCoord",
    body = body,
    encode = "raw",
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
#' \dontrun{
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

  check_vector_id(vector_id)

  vector_id <- sub("^v", "", vector_id, ignore.case = TRUE) # converts to character


  body <- paste0('[{"vectorId":', vector_id, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesDataFromVector",
    body = body,
    encode = "raw",
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
#' \dontrun{
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


  check_product_id(product_id)
  check_periods(periods)
  check_coordinate(coordinate)


  body <- paste0('[{"productId":', product_id,
                 ',"coordinate":,"', coordinate,
                 ',"latestN":', periods ,'}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods",
    body = body,
    encode = "raw",
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
#' \dontrun{
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

  check_vector_id(vector_id)
  check_periods(periods)

  vector_id <- sub("^v", "", vector_id, ignore.case = TRUE) # converts to character

  body <- paste0('[{"vectorId":', vector_id, ',"latestN":', periods, '}]')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods",
    body = body,
    encode = "raw",
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
#' @param start_release_date Release date of vector data. The returned data
#' will have been released on or after this date. The release date may lag
#' behind the reference date of the data by months or years.
#' @param end_release_date Release date of vector data. The returned data
#' will have been released on or before this date. The release date may lag
#' behind the reference date of the data by months or years.
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \dontrun{
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

  check_vector_id(vector_ids)

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

  body <- paste0('{', vectors_args, ',', times_args, '}')

  httr::POST(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange",
    body = body,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}




#' Get full table download link in CSV format
#'
#' A direct link to download the table in CSV format. This functiom
#' does not actually download the table itself.
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
#' \dontrun{
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

  check_product_id(product_id)

  if (!language %in% c("en", "fr")) {
    stop(paste0("Language must be either 'en' or 'fr'."), call. = FALSE)
  } # return actual class in message

  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/", product_id, "/", language)

  httr::GET(
    url = url,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}



#' Get full table download link in SDMX format
#'
#' A direct link to download the table in SDMX format. This functiom
#' does not actually download the table itself.
#'
#' @param product_id Product Identification number (PID) is a unique product
#' identifier for all Statistics Canada products, including large
#' multidimensional tables. The first two digits refer to a subject, the next
#' two digits refer to product type, the last four digits refer to the product
#' itself.
#'
#' @export
#'
#' @return A direct link to download the table in SDMX format..
#'
#' @examples
#' \dontrun{
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

  check_product_id(product_id)

  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadSDMX/", product_id)

  httr::GET(
    url = url,
    encode = "raw",
    httr::add_headers("Content-Type"="application/json")
  )
}





