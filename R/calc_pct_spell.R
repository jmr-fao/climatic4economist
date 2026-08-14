#' Compute spell length percentiles by month
#'
#' This function computes the spell durations of a set of daily extreme
#' indicators with [find_spell()], then summarises them into the requested
#' percentiles for each `ID` and calendar month.
#'
#' @param extr_day A data frame of daily extreme indicators, as returned by
#'   [find_extr_abs_day()] or [find_extr_rel_day()]. It must contain `ID`, `date`
#'   and the `day_abv_*` / `day_blw_*` indicator columns.
#' @param p A numeric vector of probabilities (e.g., `c(0.1, 0.5, 0.9)`)
#'          representing the percentiles to compute.
#' @param min_spell Integer. The minimum number of consecutive days required to
#'   form a spell. Default is 2. It is also used as the replacement value when a
#'   group has no spell at all.
#'
#' @return A data frame with one row per `ID` and `month`, containing one integer
#'   column per spell indicator and requested percentile, named `*_Xp` where `Xp`
#'   is the percentile (e.g. `spell_abv_90p_50p`).
#'
#' @details
#' The function follows these steps:
#' 1. Computes spell durations from the daily indicators with [find_spell()].
#' 2. Extracts the calendar `month` from `date`.
#' 3. Computes the requested percentiles of spell duration per `ID` and month.
#' 4. Rounds the resulting durations to integers.
#'
#' @seealso [find_spell()], [calc_pct_day()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extr_day <- find_extr_abs_day(df, l_thresh = 0.1)
#' calc_pct_spell(extr_day, p = c(0.1, 0.5, 0.9))
#' }

calc_pct_spell <- function(extr_day, p, min_spell = 2) {
    find_spell(extr_day, min_spell = min_spell) |>
        dplyr::mutate(month = substr(date, 6, 7),
                      .after = date) |>
        dplyr::group_by(ID, month) |>
        dplyr::reframe(
            dplyr::across(
                .cols = dplyr::matches("spell"),
                .fns = ~quantile_df(.x, p, min_spell),
                .unpack = TRUE)) |>
        dplyr::select(ID, month, dplyr::matches("[0-9]?[0-9]p$")) |>
        dplyr::mutate(dplyr::across(
            .cols = dplyr::matches("spell"),
            .fns = as.integer))
}
