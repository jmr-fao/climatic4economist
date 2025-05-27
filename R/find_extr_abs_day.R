#' Identify absolute extreme weather events
#'
#' This function detects extreme weather events based on user-defined
#' absolute upper and lower threshold values. It identifies in which days the
#' weather observation is more extreme than the thresholds and
#' calculates the amount of the excess.
#'
#' @param df A dataframe containing time-series data with column names
#'  representing dates and a variable `ID` that identifies the unique units.
#' @param u_thresh A numeric vector specifying upper threshold values. Optional,
#'   default is NULL.
#' @param l_thresh A numeric value specifying the lower threshold. Optional,
#'   default is NULL.
#' @param unit A character string specifying the unit of measure of the weather
#'   observations, (default: "unit"). E.g. "mm" for precipitation, "C" for
#'   temperature.
#'
#' @details
#' For each `ID` unit the function computes indicators for values exceeding the
#' upper and lower thresholds.
#'
#' The argument `unit` does not have implication for the computations but it
#' labels the result.
#'
#' The user can specify either or both the `u_thresh` and the `l_thresh`.
#' If none are specified the result contains just the weather observations.
#'
#' @return A dataframe containing:
#'  * for each threshold a logical column indicating if the day was more extreme
#'    events than the threshold.
#'  * for each threshold a numeric column indicating the amount of excess with
#'    respect to the threshold.
#'
#' @export
#' @import data.table
#'
#' @examples
#' df <- data.frame(ID = 1:3,
#'                  X2022.12 = c(10, 15, 5),
#'                  X2022.06 = c(12, 18, 3))
#' find_abs_extreme_pr(df, u_thresh = c(10, 20), l_thresh = 0.1)

find_extr_abs_day <- function(df, u_thresh = NULL, l_thresh = NULL, unit = "unit") {
    df_unique <- df |>
        dplyr::select(ID, dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        dplyr::rename_with(to_date) |>
        data.table::as.data.table() |>
        data.table::melt(id.vars = "ID",
                         variable.name = "date",
                         value.name = "value") |>
        data.table::setorder(ID, date)

    if (!is.null(u_thresh)) {
        day_abv <- purrr::map(u_thresh, is_above, x = df_unique) |>
            dplyr::bind_cols()
        unit_abv <- purrr::map(u_thresh, above_threshold, x = df_unique) |>
            dplyr::bind_cols()
    } else {
        day_abv <- NULL
        unit_abv <- NULL
    }

    if (!is.null(l_thresh)) {
        day_blw <- purrr::map(l_thresh, is_below, x = df_unique) |>
            dplyr::bind_cols()
        unit_blw <- purrr::map(l_thresh, below_threshold, x = df_unique) |>
            dplyr::bind_cols()
    } else {
        day_blw <- NULL
        unit_blw <- NULL
    }

    list(df_unique, day_abv, day_blw, unit_abv, unit_blw) |>
        purrr::compact() |>
        dplyr::bind_cols() |>
       # dplyr::rename({{unit}} := value) |>
        dplyr::rename_with(~gsub("unit", unit, .x)) |>
        dplyr::as_tibble()
}
