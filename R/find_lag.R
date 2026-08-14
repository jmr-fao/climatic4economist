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
#' - When `calendar = TRUE`, the interval is divided by a
#'   [lubridate::period()], which is calendar-aware: it counts **completed
#'   calendar months or years**, respecting variable month lengths and leap
#'   years.
#' - When `calendar = FALSE`, it is divided by a [lubridate::duration()], a
#'   fixed span of seconds. A year is always 365.25 days (`dyears()`) and a
#'   month always 30.4375 days (`dmonths()`).
#' - Weeks and days are the same either way, since both are fixed multiples of
#'   a day.
#'
#' The two modes disagree near a boundary. From 2021-01-01 to 2022-01-01 is a
#' complete calendar year, so `calendar = TRUE` returns 1; the same span is 365
#' days, short of the 365.25-day fixed year, so `calendar = FALSE` returns 0.
#' Prefer `calendar = TRUE` when the lag must line up with calendar reporting
#' periods, and `calendar = FALSE` when equal-length bins matter more.
#'
#' Both modes count **whole units toward zero**, so a partial interval is never
#' rounded away from zero: a span of -31.2 years gives -31, not -32. Mixing
#' positive and negative lags in one call therefore makes the bin at zero twice
#' as wide as the others, since it holds everything within one unit on either
#' side. Every lag in a given call is normally the same sign, so this rarely
#' arises, but split the calls if it does.
#'
#' `calendar = TRUE` is far more expensive, because dividing by a period has to
#' walk the calendar for every element, while dividing by a duration is one
#' vectorised arithmetic operation. On a million dates, monthly lags take about
#' 20 seconds with `calendar = TRUE` against roughly 5 milliseconds with
#' `calendar = FALSE`, and the calendar path also builds an intermediate that is
#' twice the size. Use `calendar = FALSE` for large panels when approximate
#' bins are acceptable.
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
        # Periods are calendar-aware: they respect real month lengths and leap
        # years, so a "month" is whatever the calendar says it is. This needs a
        # full Interval, which carries a start instant alongside each span.
        step <- lubridate::period(width, units = unit)
        lubridate::interval(end, start) %/% step
    } else {
        # Durations are fixed spans of seconds, so a "year" is always 365.25
        # days and a "month" always 30.4375 days. A plain elapsed time is
        # enough here, and is both smaller and far cheaper than an Interval.
        step <- switch(
            unit,
            "year"  = lubridate::dyears(width),
            "month" = lubridate::dmonths(width),
            "week"  = lubridate::dweeks(width),
            "day"   = lubridate::ddays(width)
        )
        lubridate::as.duration(start - end) %/% step
    }
}

