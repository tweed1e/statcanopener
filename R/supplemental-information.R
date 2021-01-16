# Supplemental Information

#' Get code sets
#'
#' Code Sets provide additional information to describe the information such as
#' scales, frequencies and symbols. Use method getCodeSets to access the most
#' recent version of the code sets with descriptions (English and French) for
#' all possible codes.
#'
#' @export
#' @return An httr response object
#' @examples
#' \dontrun{
#' getCodeSets()
#' }
#'
getCodeSets <- function() {
  get("getCodeSets")
}
