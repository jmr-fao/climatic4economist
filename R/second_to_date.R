#' Convert Seconds Since Epoch to Date
#'
#' This function converts a string containing numeric second values (possibly mixed with
#' non-numeric characters) into a human-readable date. It removes any letters, underscores,
#' and equal signs from the input, interprets the cleaned value as the number of seconds since
#' January 1, 1970 (the Unix epoch), and returns the corresponding date in "YYYY-MM-DD" format.
#'
#' @param x A character vector containing numeric second values mixed with non-numeric characters.
#'
#' @return A character vector of dates ("YYYY-MM-DD").
#'
#' @seealso \code{\link[lubridate]{as_date}}, \code{\link[lubridate]{as_datetime}}
#'
#' @export
#'
#' @examples
#' # Example 1: Clean and convert to date
#' second_to_date("timestamp_1704067200")
#' #> "2024-01-01"
#'
#' # Example 2: Vectorized input
#' second_to_date(c("time=1704067200", "another_time_1704153600"))
#' #> c("2024-01-01", "2024-01-02")

second_to_date <- function(x) {
    date_second <- gsub("[^0-9|\\-]|t2m", "", x = x) |> as.numeric()

    # Warn if all values are NA
    if (any(is.na(date_second))) {
        warning("Some dates are missing or it was not possible to transformed them into numeric. Returning NAs.")
    }
    lubridate::as_datetime(date_second, tz = "UTC") |>
        lubridate::as_date() |>
        as.character()
}
