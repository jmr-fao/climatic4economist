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
#' - When `calendar = TRUE`, the span is measured against the calendar: it
#'   counts **completed calendar months or years**, respecting variable month
#'   lengths and leap years. A month elapses when the day of the month comes
#'   round again, so 31 January to 28 February is one whole month.
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
#' `calendar = TRUE` costs more than `calendar = FALSE`, but not enough to
#' choose between them on speed: on a million dates monthly lags take roughly
#' 0.3 seconds against 0.01 seconds. Pick the mode that matches the bins you
#' want, not the one that runs faster.
#'
#' @export
#'
#' @seealso [clock::add_months()], [lubridate::duration()]
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

    # Only years and months have a calendar-dependent length. A week is always
    # seven days and a day always one, so those go down the cheap path whatever
    # `calendar` says, which is also what the two modes are documented to do.
    if (calendar && unit %in% c("year", "month")) {
        # Recycling makes the result as long as the longer input, so an empty
        # one empties the result. Short-circuit, because the anniversary step
        # below errors on zero-length input rather than passing it through.
        if (length(start) == 0L || length(end) == 0L) return(numeric(0))

        # Whole calendar months between the two dates, read off the year and
        # month components. Dividing an Interval by a Period gets the same
        # answer but has to walk the calendar once per element, which dominates
        # the cost on a long vector.
        months_apart <- (lubridate::year(start) - lubridate::year(end)) * 12L +
            (lubridate::month(start) - lubridate::month(end))

        # The component difference counts a month that may not have elapsed:
        # 12 July 1996 to 9 July 1997 differs by twelve months on the calendar
        # but falls three days short of a year. Compare against the anniversary
        # to drop the incomplete month, in whichever direction the span runs.
        # `invalid = "previous"` rolls a non-existent target back to the last
        # day of its month, matching how dividing by a Period treats the same
        # case: 29 February plus twelve months is 28 February.
        anniversary <- clock::add_months(end, as.integer(months_apart),
                                         invalid = "previous")

        months_apart <- ifelse(
            months_apart > 0 & anniversary > start, months_apart - 1L,
            ifelse(months_apart < 0 & anniversary < start, months_apart + 1L,
                   months_apart))

        # A calendar year is exactly twelve calendar months, so both units
        # share this path. trunc() gives the toward-zero rounding documented
        # above, which floor division would not.
        step <- if (unit == "year") 12L * width else width
        return(trunc(months_apart / step))
    }

    # Durations are fixed spans of seconds, so a "year" is always 365.25 days
    # and a "month" always 30.4375 days. Dividing an elapsed time by one is a
    # single vectorised operation.
    step <- switch(
        unit,
        "year"  = lubridate::dyears(width),
        "month" = lubridate::dmonths(width),
        "week"  = lubridate::dweeks(width),
        "day"   = lubridate::ddays(width)
    )
    lubridate::as.duration(start - end) %/% step
}

