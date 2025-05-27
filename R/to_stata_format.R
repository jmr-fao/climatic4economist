#' Convert Variable Names to Stata-Compatible Format
#'
#' This function reformat a string (e.g., a date or column name) to be
#' compatible with Stata variable naming conventions. It substitute dashes with
#' underscore, remove dots and ensures that names starting with a number are
#' prefixed with "X" (since Stata does not allow variable names to start with
#' numbers).
#'
#' @param x A character vector of variable names or dates.
#'
#' @return A character vector with modified names.
#'
#' @export
#'
#' @examples
#' to_stata_format("2024-01-01") # Returns "X2024_01_01"
#' to_stata_format("var.1")      # Returns "var1"
#' to_stata_format("123_test")   # Returns "X123_test"

to_stata_format <- function(x) {
    gsub("-", "_", x) |>
        gsub("\\.", "", x = _) |>
        gsub("(^[0-9].*)", "X\\1", x = _)
}
