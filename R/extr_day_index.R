#' Compute Extreme Day Indices for a Given Time Interval
#'
#' This function calculates indices of extreme events occurring before an
#' interview date, aggregating them over a specified time interval.
#'
#' @param df A data frame containing daily extreme values.
#' @param iteracation optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param interview A column name specifying the interview date for reference.
#' @param id A column name specifying the unique identifier for each unit
#' @param interval A string specifying the time interval (e.g., `"30days"`,
#'          `"3 months"`, `"1 year"`).
#' @param n_lags Integer. The maximum lag (in units of `interval`) to consider.
#'   Default is 0.
#'
#' @return A data frame with:
#'   - `id`: The unique identifier.
#'   - `lag`: The time lag relative to the interview date.
#'   - Summarized extreme day counts (`day_*`) and unit-based extreme values
#'      (`*_abv`, `*_blw`).
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 1, 2, 2),
#'   date = as.character(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-01", "2024-01-02")),
#'   day_abv_90p = c(0, 1, 1, 0, 0),
#'   temp_abv_90p = c(1, 2, 3, 1, 0)
#' )
#' extr_day_index(df, interview = "2024-02-01", id = ID, interval = "30days", n_lags = 1)

extr_day_index <- function(df,
                           iteracation = NULL,
                           interview,
                           id,
                           interval,
                           n_lags = 0,
                           extra_col = NULL) {

    if(missing(interview)) stop("Error: provide a date of interview or the column name with the dates of interview")
    if(missing(interval)) stop("Error: provide a time interval over which the aggregation is computer. E.g. `1 year`")
    if(missing(id)) stop("Error: provide a column name specifying the unique identifier for each unit")

    if (!is.null(iteracation)) cat("calculating extreme day index:", iteracation, "\n")

    n_period <- gsub("[a-zA-Z]| ", "", interval) |>
        as.integer()
    period <- gsub("[0-9]| ", "", interval)

    df_lag <- df |>
        dplyr::mutate(end_date := clock::date_parse(to_date({{interview}})),
                      date = clock::date_parse(as.character(date))) |>
        dplyr::filter(date < end_date) |>
        dplyr::mutate(
            lag = find_lag(date, end_date, width = n_period, unit = period),
            .after = date) |>
        dplyr::filter(lag >= 0 & lag <= n_lags)

    day_ind <- df_lag |>
        dplyr::group_by({{id}}, lag) |>
        dplyr::summarise(
            dplyr::across(
                .cols = {{extra_col}},
                .fns = unique),
            dplyr::across(
                .cols = dplyr::matches("^day"),
                .fns = list(sum = ~ sum_na_check(.x))),
            .groups = "drop")

    unit_ind <- df_lag |>
        dplyr::group_by({{id}}, lag) |>
        dplyr::summarise(dplyr::across(.cols = dplyr::matches("[^day]_abv|[^day]_blw"),
                                       .fns = list(sum = ~ sum_na_check(.x))),
                         .groups = "drop")

    dplyr::full_join(day_ind, unit_ind,
                     by = c(rlang::as_name(rlang::enquo(id)), "lag"))
}
