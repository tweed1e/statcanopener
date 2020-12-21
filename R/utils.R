#' Format date for API call
#'
#' Format date to %Y-%m-%dT%H:%M, using Statcan's time zone
#'
#' @param date A date
#'
#' @return Date with format %Y-%m-%dT%H:%M
#'
#' @examples
#' \dontrun{
#' check_vector_id(42973393)
#' }
#'
stc_time <- function(date) {
  strftime(date, "%Y-%m-%dT%H:%M", tz = "America/Toronto")
}


#' Check vector ID for errors
#'
#' Vector ID must satisfy: Vector number (i.e. 42973393 or v42973393).
#' Current max length: 10 digits Minimum length: 1.
#'
#' @param vector_id ID of the Vector that represents the time series
#'
#' @return TRUE
#'
#' @examples
#' \dontrun{
#' check_vector_id(42973393)
#' }
#'
check_vector_id <- function(vector_id) {

  if (!is.character(vector_id) & !is.numeric(vector_id)) {
    stop(paste0("vector_id must be a character or numeric vector"), call. = FALSE)
  }

  if (!any(grepl("^v?[0-9]{1,10}$", vector_id, ignore.case = TRUE))) {
    stop(paste0("vector_id must be a positive integer between 1 and 10 digits, with or without a v prefix."), call. = FALSE)
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
#'
#' @return TRUE
#'
#' @examples
#' check_product_id(1310008901)
#'
check_product_id <- function(product_id) {

  if (!is.character(product_id) & !is.numeric(product_id)) {
    stop(paste0("product_id must be a character or numeric vector"), call. = FALSE)
  }

  if (!any(grepl("^([0-9]{8}|[0-9]{10})$", product_id))) {
    stop(paste0("product_id must be an integer of length 8 or 10"), call. = FALSE)
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
#'
#' @return TRUE
#'
#' @examples
#' check_coordinate("1.1.1.36.1.0.0.0.0.0")
#'
check_coordinate <- function(coordinate) {

  if (!is.character(coordinate)) {
    stop(paste0("Coordinate must be a string"), call. = FALSE)
  }
  if (!any(grepl("^([0-9]+.){9}[0-9]+$", coordinate))) {
    stop(paste0("Exactly 10 dimensions; a fixed length. One value per dimension (i.e. 1.1.1.36.1.0.0.0.0.0)"), call. = FALSE)
  }

  return(TRUE)

}



#' Check number of periods
#'
#' Periods must be a strictly positive integer.
#'
#' @param periods Number of periods
#'
#' @return TRUE
#'
#' @examples
#' check_periods(10)
#'
check_periods <- function(periods) {

  if (!is.numeric(periods) | periods <= 0) {
    stop(paste0("Period must be a positive integer"), call. = FALSE)
  }

  if (floor(periods) != periods) {
    stop(paste0("Period must be an integer"), call. = FALSE)
  }

  return(TRUE)
}
