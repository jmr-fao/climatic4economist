#' Convert a String to a Date-Like Format
#'
#' This function transforms a string by replacing underscores (`_`) and dots (`.`)
#' with hyphens (`-`) and removing a leading "X" if present. It is useful for
#' formatting column names or other identifiers into a more readable date format.
#'
#' @param x A character vector containing strings to be converted.
#'
#' @return A character vector with transformed strings.
#'
#' @export
#'
#' @examples
#' to_date("X2023_05_01")
#' "2023-05-01"
#'
#' to_date("X2023.12.31")
#' "2023-12-31"
#'
#' to_date("2023/12/31")
#' "2023-12-31"

to_date <- function(x) {
    gsub("_|\\.|/", "-", x) |>
        gsub("^X", "", x = _)
}
