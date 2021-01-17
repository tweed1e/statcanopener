wds_url <- "https://www150.statcan.gc.ca/t1/wds/rest/"

#' Format json for API call
#'
#' Format json for API call
#'
#' @param ... List of parameters
#' @return JSON
#' @examples
#' \dontrun{
#'
#' }
#'
format_wds_json <- function(...) {
  UseMethod("format_wds_json")
}

format_wds_json.default <- function(...) {
  jsonlite::toJSON(list(list(...)), auto_unbox = TRUE)
}

format_wds_json.list <- function(...) {
  jsonlite::toJSON(..., auto_unbox = TRUE)
}

format_wds_json.data.frame <- function(...) {
  jsonlite::toJSON(..., auto_unbox = TRUE)
}

#' Format date for API call
#'
#' Format date to %Y-%m-%dT%H:%M, using Statcan's time zone
#'
#' @param date A date
#' @return Date with format %Y-%m-%dT%H:%M
#' @examples
#' \dontrun{
#' stc_time("1999-01-27")
#' }
#'
stc_time <- function(date) {
  strftime(date, "%Y-%m-%dT%H:%M", tz = "America/Toronto")
}

#' POST utility
#'
#' @param url_func name of API function to be appended to wds_url
#' @param ... body of POST request, as named arguments or a dataframe
#' @return httr response object
post <- function(url_func, ...) {
  httr::POST(
    url = paste0(wds_url, url_func),
    body = format_wds_json(...),
    encode = "raw",
    httr::add_headers("Content-Type" = "application/json")
  )
}

#' GET utility
#'
#' @param url_func Name of API function to be appended to wds_url
#' @return httr response object
get <- function(url_func) {
  httr::GET(
    url = paste0(wds_url, url_func),
    encode = "raw",
    httr::add_headers("Content-Type" = "application/json")
  )
}


#' Check vector ID for errors
#'
#' Vector ID must satisfy: Vector number (i.e. 42973393).
#' Current max length: 10 digits Minimum length: 1.
#'
#' @param vector_id ID of the Vector that represents the time series
#' @return TRUE
#' @examples
#' \dontrun{
#' check_vector_id(42973393)
#' }
#'
check_vector_id <- function(vector_id) {
  if (!is.numeric(vector_id)) {
    stop(paste0("Vector ID must be a positive integer between 1 and 10 digits"), call. = FALSE)
  }

  if (!all(vector_id > 0)) {
    stop(paste0("Vector ID must be a positive integer between 1 and 10 digits"), call. = FALSE)
  }

  if (!any(grepl("^[0-9]{1,10}$", vector_id))) {
    stop(paste0("Vector ID must be a positive integer between 1 and 10 digits"), call. = FALSE)
  }

  return(TRUE)
}



#' Check product ID for errors
#'
#' Product ID must satisfy: 10-digit identifier (i.e. 1310008901).
#' First two digits are the Subject (i.e. 13=Health). Digits 3 and 4 are the
#' product type (i.e. 10=table/cube). Digits 5 to 8 are the sequential numbers
#' within the subject (i.e. 0089). Digits 9 and 10 are optional, to identify
#' the simple view of a table/cube (i.e. 01)
#'
#' @param product_id ID of the product/table
#' @return TRUE
#' @examples
#' \dontrun{
#' check_product_id(1310008901)
#' }
check_product_id <- function(product_id) {
  if (!is.numeric(product_id)) {
    stop(paste0("Product ID must be a numeric vector"), call. = FALSE)
  }

  if (!any(grepl("^([0-9]{8}|[0-9]{10})$", product_id))) {
    stop(paste0("Product ID must be an integer of length 8 or 10"), call. = FALSE)
  }

  return(TRUE)
}


#' Check coordinate for errors
#'
#' Coordinate must satisfy: String.	Concatenation of the member ID values for
#' each dimension. (refer to the TableMetadata datastructure for information
#' about members). Maximum of 10 dimensions - a fixed length.
#' One value per dimension (i.e. 1.1.1.36.1.0.0.0.0.0)
#'
#' @param coordinate coordinate of the data point
#' @return TRUE
#' @examples
#' \dontrun{
#' check_coordinate("1.1.1.36.1.0.0.0.0.0")
#' }
check_coordinate <- function(coordinate) {
  if (!is.character(coordinate)) {
    stop(paste0("Coordinate must be a string"), call. = FALSE)
  }

  if (!any(grepl("^([0-9]+.){9}[0-9]+$", coordinate))) {
    stop(paste0("Coordinate must have exactly 10 dimensions; a fixed length.
                One value per dimension (i.e. 1.1.1.36.1.0.0.0.0.0)"),
         call. = FALSE)
  }

  return(TRUE)
}



#' Check number of periods
#'
#' Periods must be a strictly positive integer.
#'
#' @param periods Number of periods
#' @return TRUE
#' @examples
#' \dontrun{
#' check_periods(10)
#' }
check_periods <- function(periods) {
  if (!is.numeric(periods) | any(periods <= 0)) {
    stop(paste0("Period must be a positive integer"), call. = FALSE)
  }

  if (any(floor(periods) != periods)) {
    stop(paste0("Period must be an integer"), call. = FALSE)
  }

  return(TRUE)
}


check_vector_values <- function(value) {
  if (length(value) == 0) {
    stop(paste0("Vector is empty"), call. = FALSE)
  }
  if (!is.numeric(value)) {
    stop(paste0("Vector is not numeric"), call. = FALSE)
  }
}



#' Extract vector data from httr content response
#'
#' Extract vector data from httr content response and return as data.frame
#'
#' @param content_vector httr content response from getBulkVectorDataByRange call
#' @return data.frame with 3 columns: vector_id, ref_date, value
#'
#' @examples
#' \dontrun{
#' response <- getBulkVectorDataByRange("v113411623")
#' vector_content <- httr::content(response)
#' extract_vector(vector_content)
#' }
#'
extract_vector <- function(content_vector) {

  # test that this is httr content from a getBulkVectorByRange call // ugh, it's not
  # check ref_dates, check value, check ID name

  vector_id <- content_vector$object[["vectorId"]]
  vector_values <- content_vector$object[["vectorDataPoint"]]

  ref_date <- sapply(vector_values, `[[`, "refPer")
  value <- sapply(vector_values, `[[`, "value")

  check_vector_id(vector_id)
  check_vector_values(value)

  ref_date <- as.Date(ref_date) # throws error if not in the right format?

  data.frame(vector_id, ref_date, value, stringsAsFactors = FALSE)
}

# Next up: filter and process the vector data
# And add functionality to add multiple things to API call.
# might need to change body call to the list/encode json thing
