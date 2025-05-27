#' Compute the number of time intervals between two dates
#'
#' This function calculates the number of complete time intervals (e.g., years
#' or months) between a start and an end date, based on a specified interval
#' width.
#'
#' @param start A `Date` or `POSIXt` object representing the start date.
#' @param end A `Date` or `POSIXt` object representing the end date.
#' @param width An integer specifying the width of the time interval. Defaults
#'   to 1.
#' @param unit A character string specifying the unit of time to measure
#'   (`"year"`, `"month"`, etc.). Defaults to `"year"`. Must be a valid unit
#'   recognized by [lubridate::time_length].
#'
#' @return An integer representing the number of complete intervals of the
#'   specified width between the start and end dates.
#'
#' @export
#'
#' @seealso [lubridate::time_length()] [lubridate::interval()]
#'
#' @examples
#' # Example: Find the number of full years between two dates
#' find_lag(as.Date("2020-01-01"), as.Date("2025-01-01"))
#'
#' # Example: Find the number of 6-month intervals
#' find_lag(as.Date("2020-01-01"), as.Date("2023-01-01"),
#'          width = 6, unit = "months")
#'

find_lag <- function(start, end, width = 1, unit = "year") {
    n_unit <- lubridate::interval(start, end) |>
        lubridate::time_length(unit) |>
        floor() |>
        as.integer()

    n_unit%/%width
}


# find_lag <- function(start, end, width = 1, unit = "year") {
#     if (unit == "year") {
#         start_y <- as.integer(format(start, "%Y"))
#         end_y   <- as.integer(format(end,   "%Y"))
#         (end_y - start_y) %/% width
#
#     } else if (unit == "month") {
#         start_y <- as.integer(format(start, "%Y"))
#         end_y   <- as.integer(format(end,   "%Y"))
#         start_m <- as.integer(format(start, "%m"))
#         end_m   <- as.integer(format(end,   "%m"))
#         ((end_y - start_y) * 12 + (end_m - start_m)) %/% width
#
#     } else if (unit == "day") {
#         as.integer(difftime(end, start, units = "days")) %/% width
#
#     } else {
#         stop("Unsupported unit. Use 'year', 'month', or 'day'.")
#     }
# }
