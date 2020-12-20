




stc_time <- function(date) {
  strftime(date, "%Y-%m-%dT%H:%M", tz = "America/Toronto")
}


#' Download a vector
#'
#' Download a vector
#'
#' @param vectors
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

  vector_ids <- sub("^v", "", vector_ids) # converts to character

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

  httr::POST("https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange",
             body = body,
             encode="json",
             httr::add_headers("Content-Type"="application/json"))
}



# Product Change Listings

# getChangedSeriesList
# GET URL:
#   https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesList
getChangedSeriesList <- function() {
  httr::GET("https://www150.statcan.gc.ca/t1/wds/rest/getChangedSeriesList",
            encode="json",
            httr::add_headers("Content-Type"="application/json"))
}

# getChangedCubeList
# GET URL:
#   https://www150.statcan.gc.ca/t1/wds/rest/getChangedCubeList/2017-12-07
getChangedCubeList <- function() {
  httr::GET("https://www150.statcan.gc.ca/t1/wds/rest/getChangedCubeList/2017-12-07",
            encode="json",
            httr::add_headers("Content-Type"="application/json"))
}

# Cube Metadata and Series Information:

# getCubeMetadata
# POST URL:
#   https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata
#
# POST BODY:
#   [{"productId":35100003}]
getCubeMetadata <- function(product_id) {
  body <- paste0('[{"productId:"', product_id, ']}')

  httr::POST("https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata",
             body = body,
             encode="json",
             httr::add_headers("Content-Type"="application/json"))
}
getSeriesInfoFromCubePidCoord <- function(product_id, coordinate) {
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromCubePidCoord
  #
  # POST BODY:
  #   [{"productId": 35100003, "coordinate": "1.12.0.0.0.0.0.0.0.0"}]

  body <- paste0('[{"productId:"', product_id, ',"coordinate:","', coordinate, ']}')

  httr::POST("https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromCubePidCoord",
             body = body,
             encode="json",
             httr::add_headers("Content-Type"="application/json"))
}
getSeriesInfoFromVector <- function(vector_id) {
  # POST URL:
  #   https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector
  #
  # POST BODY:
  #   [{"vectorId":32164132}]
  body <- paste0('[{"vectorId:"', vector_id, ']}')

  httr::POST("https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromCubePidCoord",
             body = body,
             encode="json",
             httr::add_headers("Content-Type"="application/json"))
}


# getAllCubesList
# Users can query the output database to provide a complete inventory of data tables available through this Statistics Canada API. This command accesses a comprehensive list of details about each table including information at the dimension level.
#
# GET URL:
#   https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList
getAllCubesList <- function() {
  httr::GET("https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList",
            encode="json",
            httr::add_headers("Content-Type"="application/json"))
}

# getAllCubesListLite
# Users can query the output database to provide a complete inventory of data tables available through this Statistics Canada API. This command accesses a list of details about each table.  Unlike getAllCubesList, this method does not return dimension or footnote information.
#
# GET URL:
#   https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesListLite
getAllCubesListLite <- function() {
  httr::GET("https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesListLite",
            encode="json",
            httr::add_headers("Content-Type"="application/json"))
}


# Data Access; data changes for today, over time and full table
getChangedSeriesDataFromCubePidCoord <- function() {}
getChangedSeriesDataFromVector <- function() {}
getDataFromCubePidCoordAndLatestNPeriods <- function() {}
getDataFromVectorsAndLatestNPeriods <- function() {}
# getBulkVectorDataByRange <- function() {}

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
getFullTableDownloadCSV <- function(product_id, language = c("en", "fr")) {

  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/", product_id, "/", language)

  httr::GET(url,
             encode="json",
             httr::add_headers("Content-Type"="application/json"))
}

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

getFullTableDownloadSDMX <- function(product_id) {

  url <- paste0("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadSDMX/", product_id)

  httr::GET(url,
            encode="json",
            httr::add_headers("Content-Type"="application/json"))
}

# Supplemental Information
getCodeSets <- function() {}

# vectors <- c("113413955","113411623")


# data <- httr::content(res)


