#' Convert a character vector to a clock object
#'
#' This function takes a character vector, replaces underscores or periods with
#' dashes, removes any leading 'X', and parses the resulting string as a
#' date-time object.
#'
#' @param x A character vector representing dates in various formats, with
#'   underscores or periods.
#'
#' @return A vector of parsed date-time objects of class `clock` from the
#'   `clock` package.
#'
#' @export
#'
#' @examples
#' # Example input with underscores
#' x1 <- c("X2022_01_01", "X2023_02_02")
#' character_to_clock(x1)
#'
#' # Example input with periods
#' x2 <- c("X2022.01.01", "X2023.02.02")
#' character_to_clock(x2)

character_to_clock <- function(x) {
    to_date(x) |>
        clock::date_parse()
}
