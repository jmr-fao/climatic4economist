#' Extract Spell Index Statistics Over a Specified Interval
#'
#' This function calculates summary statistics (max, mean, sum, and count) of
#' spell-related variables within a given time interval before an interview
#' date.
#'
#' @param df A dataframe containing an interview date, event dates, and
#'   spell-related variables.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param interview The column name representing the interview date.
#' @param id The column name representing unique identifiers for individuals or
#'   locations.
#' @param interval A character string specifying the time interval (e.g., "12m"
#'   for 12 months).
#' @param n_lags (Optional) The number of lag periods to consider. Default is 0
#'   (only the defined interval).
#' @param extra_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional columns
#'   carried through unchanged. They must be constant within each `id`/`lag`
#'   group, since they are reduced with `unique()`.
#'
#' @return A dataframe summarizing spell-related variables for each ID and lag
#'   period.
#'
#' @export
#'
#' @examples
#' extr_spell_index(df, interview = "interview_date", id = "household_id",
#'                  interval = "12m", n_lags = 3)

extr_spell_index <- function(df,
                             iteration = NULL,
                             interview,
                             id,
                             interval,
                             n_lags = 0,
                             extra_col = NULL) {
    if(missing(interview)) stop("Error: provide a date of interview or the column name with the dates of interview")
    if(missing(interval)) stop("Error: provide a time interval over which the aggregation is computer. E.g. `1 year`")
    if(missing(id)) stop("Error: provide a column name specifying the unique identifier for each unit")

    if (!is.null(iteration)) cat("Computing extreme spell index:", iteration, "\n")

    n_period <- gsub("[a-zA-Z]| ", "", interval) |>
        as.integer()
    period <- gsub("[0-9]| |s$", "", interval)

    # validate period
    period <- match.arg(tolower(period), c("year", "month", "week", "day"))

    parse_if_needed <- function(x) {
        if (inherits(x, c("Date", "POSIXt"))) x else clock::date_parse(x)
    }

    df_date <- df |>
        dplyr::mutate(end_date := parse_if_needed({{interview}}),
                      date = clock::date_parse(as.character(date)))

    # compute min_date shifted back
    n_shift <- -(n_period * (n_lags + 1))
    min_end <- min(df_date$end_date, na.rm = TRUE)

    min_date <- switch(
        period,
        "year"  = clock::add_years(min_end, n_shift),
        "month" = clock::add_months(min_end, n_shift),
        "week"  = clock::add_weeks(min_end, n_shift),
        "day"   = clock::add_days(min_end, n_shift)
    )

    df_lag <- df_date |>
        dplyr::filter(date < end_date & date >= min_date) |>
        dplyr::mutate(
            lag = find_lag(end_date, date, width = n_period, unit = period),
            .after = date) |>
        dplyr::filter(lag >= 0 & lag <= n_lags)

    df_lag |>
        dplyr::group_by({{id}}, lag) |>
        dplyr::summarise(dplyr::across(.cols = {{extra_col}},
                                       .fns = unique),
                         dplyr::across(.cols = dplyr::matches("^spell"),
                                       .fns = list(max  = ~ max_na_check(.x),
                                                   mean = ~ mean_na_check(.x),
                                                   sum  = ~ sum_na_check(.x),
                                                   n    = ~ sum(!is.na(.x)))),
                         .groups = "drop")
}
