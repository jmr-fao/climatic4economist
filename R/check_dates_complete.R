#' Check Completeness of a Date Sequence
#'
#' This function checks whether a vector of dates is sequential without missing
#' years, months, or days. It can return detailed information about missing
#' units and an overall continuity flag.
#'
#' @param dates A vector of dates (class `Date`, `POSIXct`, or character convertible to Date).
#' @param freq Character vector specifying which levels to check: `"year"`, `"month"`, `"day"`.
#'   Default is all three.
#' @param return_missing Logical. If TRUE, returns exact missing years, months, and days.
#'
#' @return A list containing:
#'   \describe{
#'     \item{all_continuous}{TRUE if the dates are fully sequential at requested frequency.}
#'     \item{year_check}{TRUE if years are continuous, FALSE otherwise.}
#'     \item{month_issues}{Data frame of years with missing months or incorrect days.}
#'     \item{day_issues}{Data frame of year-month combinations with missing days.}
#'     \item{missing}{Optional list of exact missing units (years, months, days), only if `return_missing = TRUE`.}
#'   }
#'
#' @examples
#' \dontrun{
#' dates <- seq.Date(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
#' check_dates_complete(dates)
#' }
#'
#' @export
check_dates_complete <- function(dates, freq = c("year", "month", "day"), return_missing = FALSE) {
    # ensure Date
    dates <- lubridate::as_date(dates)

    out <- list()

    # year check. ----
    if ("year" %in% freq) {
        years <- sort(unique(lubridate::year(dates)))
        min_year <- min(years)
        max_year <- max(years)
        year_check <- all(seq(min_year, max_year) %in% years)
        out$year_check <- year_check

        if (return_missing) {
            out$missing_years <- setdiff(seq(min_year, max_year), years)
        }
    }

    # month check ----
    if ("month" %in% freq) {
        month_summary <- data.frame(date = dates) |>
            dplyr::mutate(year = lubridate::year(date),
                          month = lubridate::month(date)) |>
            dplyr::group_by(year) |>
            dplyr::summarise(n_month = dplyr::n_distinct(month),
                             n_day   = dplyr::n_distinct(date),
                             .groups = "drop") |>
            dplyr::mutate(is_leap = lubridate::leap_year(year),
                          flag_month = n_month != 12,
                          flag_day   = !((n_day == 365) | (n_day == 366 & is_leap))) |>
            dplyr::filter(flag_month | flag_day)

        out$month_issues <- month_summary

        if (return_missing && nrow(month_summary) > 0) {
            missing_months <- lapply(month_summary$year,
                                     \(y) setdiff(1:12,
                                                  lubridate::month(dates[lubridate::year(dates) == y])))
            names(missing_months) <- month_summary$year
            out$missing_months <- missing_months
        }
    }

    # day check ----
    if ("day" %in% freq) {
        day_summary <- data.frame(date = dates) |>
            dplyr::mutate(year = lubridate::year(date),
                          month = lubridate::month(date)) |>
            dplyr::group_by(year, month) |>
            dplyr::summarise(n_day = dplyr::n_distinct(date),
                             .groups = "drop") |>
            dplyr::mutate(expected_days = lubridate::days_in_month(lubridate::make_date(year, month, 1)),
                          flag_day = n_day != expected_days) |>
            dplyr::filter(flag_day)

        out$day_issues <- day_summary

        if (return_missing && nrow(day_summary) > 0) {
            missing_days <- lapply(1:nrow(day_summary), function(i) {
                yr <- day_summary$year[i]
                mo <- day_summary$month[i]
                seq.Date(lubridate::make_date(yr, mo, 1),
                         lubridate::make_date(yr, mo, day_summary$expected_days[i]),
                         by = "day") |>
                    setdiff(dates)})

            names(missing_days) <- paste(day_summary$year,
                                         day_summary$month,
                                         sep = "-")
            out$missing_days <- missing_days
        }
    }

    # overall continuity
    out$all_continuous <- all(
        ifelse("year" %in% freq, out$year_check, TRUE),
        ifelse("month" %in% freq, nrow(out$month_issues) == 0, TRUE),
        ifelse("day" %in% freq, nrow(out$day_issues) == 0, TRUE)
    )

    return(out)
}
