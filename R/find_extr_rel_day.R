#' Identify relative extreme days
#'
#' This function detects relative extreme values in a dataset based on
#' percentile thresholds. It identifies days where values exceed or fall below
#' specified percentiles and calculates the excess or deficit relative to these
#' thresholds.
#'
#' @param df A dataframe containing an ID column and date-based columns.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param u_thresh (Optional) A dataframe containing upper percentile thresholds
#'   for each ID and month. Typically computed using `calc_pct_day()`.
#' @param l_thresh (Optional) A dataframe containing lower percentile thresholds
#'   for each ID and month. Typically computed using `calc_pct_day()`.
#' @param unit (Optional) A character string specifying the unit name to replace
#'   "unit" in output columns.
#'
#' @return A dataframe with columns indicating whether values exceeded or fell
#'         below percentile thresholds, along with the corresponding differences.
#'  - `day_abv_Xp`: Boolean indicating if weather exceeds the threshold
#'    `Xp`.
#'  - `unit_abv_Xp`: The amount by which weather exceeds the upper threshold
#'    `Xp`.
#'  - `day_abv_max_Xp`: Boolean indicating if weather exceeds the max
#'    upper threshold among all the months.
#'  - `day_blw_Xp`: Boolean indicating if weather exceeds the lower threshold
#'    `Xp`.
#'  - `unit_blw_Xp`: The amount by which weather exceeds the lower threshold
#'    `Xp`.
#'  - `day_blw_max_Xp`: Boolean indicating if weather exceeds the min
#'    lower threshold among all the months.
#'
#' @export
#' @import data.table

find_extr_rel_day <- function(df,
                              iteration = NULL,
                              u_thresh = NULL,
                              l_thresh = NULL,
                              unit = "unit") {

    if (!is.null(iteration)) cat("findig extreme day:", iteration, "\n")

    df_long <- df |>
        dplyr::select(ID, dplyr::matches("[0-9]{4}")) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        dplyr::rename_with(to_date) |>
        data.table::as.data.table() |>
        data.table::melt(id.vars = "ID",
                         variable.name = "date",
                         value.name = "value")

    # melt returns the former column names as a factor ordered by column
    # position; parse before sorting so rows end up in chronological order
    df_long[, date := parse_date_label(date)]
    data.table::setorder(df_long, ID, date)

    df_long[, month := month_label(date), ]

    if (!is.null(u_thresh)) {
        abv <- u_thresh |>
            dplyr::select(ID, month, dplyr::matches("_[0-9]?[0-9]p$")) |>
            dplyr::distinct() |>
            dplyr::full_join(df_long, by = c("ID", "month"),
                             relationship = "one-to-many") |>
            dplyr::mutate(
                dplyr::across(
                    # ^.{1,4} start with any word from 1 to 4 charterer (e.g. day or yr)
                    # [0-9]?[0-9]p$ end with p preceded by one or two digit (e.g. 1p or 95p)
                    .cols  = dplyr::matches("^.{1,3}_[0-9]?[0-9]p$"), # e.g day_95p or abv_01p
                    .fns   = ~ value > .x,
                    .names = "day_abv_{.col}"),
                dplyr::across(
                    .cols  = dplyr::matches("^.{1,3}_[0-9]?[0-9]p$"),
                    .fns   = ~ dplyr::if_else(value > .x, value - .x, 0),
                    .names = "unit_abv_{.col}")
                ) |>
            dplyr::rename_with(
                .cols = dplyr::matches("_day_"),
                .fn = ~ gsub("_day_", "_", .x)
                ) |>
            dplyr::select(-c(month, value, dplyr::matches("^.{1,3}_[0-9]?[0-9]p$")))

    } else {
        abv <- NULL
    }

    if(!is.null(l_thresh)) {
        blw <- l_thresh |>
            dplyr::select(ID, month, dplyr::matches("_[0-9]?[0-9]p$")) |>
            dplyr::distinct() |>
            dplyr::full_join(df_long, by = c("ID", "month"),
                             relationship = "one-to-many") |>
            dplyr::mutate(
                dplyr::across(
                    # ^.{1,4} start with any word from 1 to 4 charterer (e.g. day or yr)
                    # [0-9]?[0-9]p$ end with p preceded by one or two digit (e.g. 1p or 95p)
                    .cols  = dplyr::matches("^.{1,3}_[0-9]?[0-9]p$"),
                    .fns   = ~ value < .x,
                    .names = "day_blw_{.col}"),
                dplyr::across(
                    .cols  = dplyr::matches("^.{1,3}_[0-9]?[0-9]p$"),
                    .fns   = ~ dplyr::if_else(value < .x, .x - value, 0),
                    .names = "unit_blw_{.col}")
                ) |>
            dplyr::rename_with(
                .cols = dplyr::matches("_day_"),
                .fn = ~ gsub("_day_", "_", .x)
                ) |>
            dplyr::select(-c(month, value, dplyr::matches("^.{1,3}_[0-9]?[0-9]p$")))

    } else {
        blw <- NULL
    }

    list(df_long, abv, blw) |>
        purrr::keep(is.data.frame) |>
        purrr::reduce(dplyr::full_join, by = c("ID", "date")) |>
        dplyr::select(-month) |>
        dplyr::rename_with(~gsub("unit", unit, .x)) |>
        dplyr::as_tibble()
}
