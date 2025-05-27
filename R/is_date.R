#' Check if a value is a valid date object
#'
#' This function checks whether a given value can be parsed as a valid date.
#'
#' @param mydate character. A character string representing a date.
#'
#' @return A logical value indicating whether the input is a valid date.
#'
#' @export
#'
#' @examples
#' is_date("2023-01-01")  # TRUE
#' is_date("not a date")  # FALSE

is_date <- function(mydate) {
    tryCatch(!is.na(as.Date(mydate,
                            tryFormats = c("%Y-%m-%d", "%Y/%m/%d",
                                           "%d-%m-%Y", "%m-%d-%Y"))),
             error = function(err) {FALSE})
}
