#' Extract Spell Index Statistics Over a Specified Interval
#'
#' This function calculates summary statistics (max, mean, sum, and count) of
#' spell-related variables within a given time interval before an interview
#' date.
#'
#' @param df A dataframe containing an interview date, event dates, and
#'   spell-related variables.
#' @param interview The column name representing the interview date.
#' @param id The column name representing unique identifiers for individuals or
#'   locations.
#' @param interval A character string specifying the time interval (e.g., "12m"
#'   for 12 months).
#' @param n_lags (Optional) The number of lag periods to consider. Default is 0
#'   (only the defined interval).
#'
#' @return A dataframe summarizing spell-related variables for each ID and lag
#'   period.
#'
#' @export
#'
#' @examples
#' extr_spell_index(df, interview = "interview_date", id = "household_id",
#'                  interval = "12m", n_lags = 3)

extr_spell_index <- function(df, interview, id, interval, n_lags = 0, extra_col = NULL) {
    n_period <- gsub("[a-zA-Z]| ", "", interval) |>
        as.integer()
    period <- gsub("[0-9]| ", "", interval)

    df_lag <- df |>
        dplyr::mutate({{interview}} := character_to_clock({{interview}}),
                      date = character_to_clock(date)) |>
        dplyr::filter(date < {{interview}}) |>
        dplyr::mutate(
            lag = find_lag(date, {{interview}}, width = n_period, unit = period),
            .after = date) |>
        dplyr::filter(lag >= 0 & lag <= n_lags)

    df_lag |>
        dplyr::group_by({{id}}, lag) |>
        dplyr::summarise(dplyr::across(.cols = dplyr::matches("^spell"),
                                       .fns = list(max  = ~ max_na_check(.x),
                                                   mean = ~ mean_na_check(.x),
                                                   sum  = ~ sum_na_check(.x),
                                                   n    = ~ sum(!is.na(.x)))),
                         .groups = "drop")
}
