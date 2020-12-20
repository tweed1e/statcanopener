# Supplemental Information

#' Get code sets
#'
#' Code Sets provide additional information to describe the information such as
#' scales, frequencies and symbols. Use method getCodeSets to access the most
#' recent version of the code sets with descriptions (English and French) for
#' all possible codes.
#'
#'
#' @export
#'
#' @return A json object.
#'
#' @examples
#' \donttest{
#' getCodeSets()
#' }
#'
getCodeSets <- function() {
  httr::GET(
    url = "https://www150.statcan.gc.ca/t1/wds/rest/getCodeSets",
    encode="json",
    httr::add_headers("Content-Type"="application/json")
  )
}
