#' Parse an interval string into a count and a unit
#'
#' Splits strings such as `"1 year"`, `"3 months"` or `"30days"` into the number
#' of periods and the period unit. Digits and whitespace are stripped to find
#' the unit, letters and whitespace to find the count, so both spaced and
#' unspaced forms are accepted. A trailing plural `"s"` is dropped.
#'
#' @param interval A character string, e.g. `"1 year"` or `"6 months"`.
#'
#' @return A list with `n`, an integer count, and `unit`, one of `"year"`,
#'   `"month"`, `"week"` or `"day"`.
#'
#' @noRd
parse_interval <- function(interval) {
    n <- as.integer(gsub("[a-zA-Z]|\\s", "", interval))
    if (is.na(n)) {
        stop("`interval` must contain a number, e.g. \"1 year\".", call. = FALSE)
    }

    unit <- gsub("[0-9]|\\s|s$", "", interval)
    unit <- match.arg(tolower(unit), c("year", "month", "week", "day"))

    list(n = n, unit = unit)
}

#' Shift dates by a whole number of calendar periods
#'
#' Wraps the `clock::add_*()` family behind a single unit argument. Years and
#' months use `invalid = "previous"`, so shifting onto a date that does not
#' exist (31 January minus one month, or 29 February in a common year) lands on
#' the last valid day of the target month rather than erroring. Weeks and days
#' are exact multiples of a day and cannot land on an invalid date, so they take
#' no `invalid` argument.
#'
#' @param date A `Date` vector.
#' @param n An integer vector of periods to add. Negative values shift back.
#' @param unit One of `"year"`, `"month"`, `"week"` or `"day"`.
#'
#' @return A `Date` vector the same length as `date`.
#'
#' @noRd
shift_by_period <- function(date, n, unit) {
    switch(
        unit,
        "year"  = clock::add_years(date,  n, invalid = "previous"),
        "month" = clock::add_months(date, n, invalid = "previous"),
        "week"  = clock::add_weeks(date,  n),
        "day"   = clock::add_days(date,   n)
    )
}

#' Build the reference windows for each interview
#'
#' Produces one row per unique combination of grouping key, interview date and
#' lag, with the half-open window `(w_start, w_end]` that the lag refers to.
#'
#' Lag `L` covers the dates lying between `L + 1` and `L` periods before the
#' interview, matching [find_lag()]: a date belongs to lag `L` when exactly
#' `L` complete periods separate it from the interview. Lag 0 additionally
#' excludes the interview date itself, so its upper bound is the day before.
#'
#' The window table is built from the *distinct* key/interview pairs rather than
#' from the survey rows. Two households sharing a location and an interview date
#' have the same window and therefore the same indicator values, so the
#' aggregation is done once and broadcast back afterwards.
#'
#' @param survey A data frame with the grouping key and interview date columns.
#' @param by <[`data-masking`][rlang::args_data_masking]> The column linking the
#'   survey to the indicator data, typically the location `ID`.
#' @param interview <[`data-masking`][rlang::args_data_masking]> The column
#'   holding the interview dates.
#' @param n_period An integer, the width of one reference period.
#' @param period The period unit, one of `"year"`, `"month"`, `"week"`, `"day"`.
#' @param n_lags An integer, the highest lag to build.
#'
#' @return A tibble with the `by` column, the `interview` column, `lag`,
#'   `w_start` and `w_end`.
#'
#' @noRd
make_lag_windows <- function(survey, by, interview, n_period, period, n_lags) {
    lags <- seq.int(0L, as.integer(n_lags))

    survey |>
        dplyr::distinct({{by}}, {{interview}}) |>
        dplyr::filter(!is.na({{interview}}), !is.na({{by}})) |>
        tidyr::expand_grid(lag = lags) |>
        dplyr::mutate(
            w_start = shift_by_period({{interview}}, -n_period * (lag + 1L), period),
            w_end   = shift_by_period({{interview}}, -n_period * lag, period),
            # lag 0 ends the day before the interview: the interview date itself
            # is never part of its own reference period
            w_end   = dplyr::if_else(lag == 0L, {{interview}} - 1L, w_end))
}
