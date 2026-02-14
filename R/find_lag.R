#' Compute the number of complete time intervals between two dates
#'
#' This function calculates the number of complete time intervals (e.g., years,
#' months, weeks, or days) between a start and an end date. It supports both
#' calendar-aware intervals (respecting actual month lengths and leap years)
#' and fixed-duration intervals (approximating months and years with average
#' lengths in days).
#'
#' @param start A `Date` or `POSIXt` object representing the start date.
#' @param end A `Date` or `POSIXt` object representing the end date.
#' @param width An integer specifying the width of the time interval. Defaults
#'   to 1.
#' @param unit A character string specifying the unit of time to measure
#'   (`"year"`, `"month"`, `"week"`, `"day"`). Defaults to `"year"`.
#' @param calendar Logical; if `TRUE`, computes calendar-aware intervals using
#'   real months and years. If `FALSE`, computes fixed-duration intervals
#'   using average days (365.25 for years, 30.4375 for months). Defaults to
#'   `TRUE`.
#'
#' @return An integer (or numeric vector) representing the number of complete
#'   intervals of the specified width between the start and end dates.
#'
#' @details
#' - When `calendar = TRUE`, the function counts **completed calendar months
#'   or years**, taking into account variable month lengths and leap years.
#' - When `calendar = FALSE`, it uses **average month/year lengths in days**
#'   to compute fixed-duration intervals.
#' - Weeks and days are always computed as fixed durations.
#'
#' @export
#'
#' @seealso [lubridate::interval()]
#'
#' @examples
#' # Calendar-aware: number of full years between two dates
#' find_lag(as.Date("2020-01-01"), as.Date("2025-01-01"))
#'
#' # Calendar-aware: number of 6-month intervals
#' find_lag(as.Date("2020-01-01"), as.Date("2023-01-01"),
#'          width = 6, unit = "month")
#'
#' # Fixed-duration: approximate number of months using average 30.4375 days
#' find_lag(as.Date("2020-01-01"), as.Date("2023-01-01"),
#'          width = 6, unit = "month", calendar = FALSE)
#'
#' # Calendar-aware: rolling month indices for daily dates
#' dates <- seq.Date(as.Date("2023-11-24"), as.Date("2025-11-24"), by = "day")
#' reference <- as.Date("2025-11-24")
#' find_lag(dates, reference, unit = "month")
#'
find_lag <- function(start,
                     end,
                     width = 1,
                     unit = c("year", "month", "week", "day"),
                     calendar = TRUE) {

    unit <- match.arg(unit)

    start <- lubridate::as_date(start)
    end   <- lubridate::as_date(end)

    if (calendar) {
        # Calendar-aware intervals
        lag <- switch(
            unit,
            "year"  = lubridate::interval(start, end) %/% lubridate::years(width),
            "month" = lubridate::interval(start, end) %/% lubridate::months(width),
            "week"  = as.numeric(start - end) %/% (7 * width),
            "day"   = as.numeric(start - end) %/% width
        )
    } else {
        # Fixed-duration intervals (average days)
        interval_days <- switch(
            unit,
            "year"  = 365.25 * width,
            "month" = 30.4375 * width,
            "week"  = 7 * width,
            "day"   = width
        )

        lag <- as.numeric(start - end) %/% interval_days
    }

    return(lag)
}

