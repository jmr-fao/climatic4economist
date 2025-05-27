#' Aggregate Data to Administrative Divisions
#'
#' This function aggregates daily and spell-based extreme event indicators to the level
#' of administrative divisions using a weighted mean, where weights are based on the
#' `coverage_fraction` of each observation.
#'
#' @param df A data frame containing extreme event indicators, administrative division IDs,
#'   and coverage fractions.
#'
#' @return A data frame grouped by administrative division (`ID_adm_div`) and `date`,
#'   with aggregated values for daily (`^day`) and spell (`^spell`) indicators.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID_adm_div = c(1, 1, 2, 2),
#'   date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02")),
#'   day_abv_90p = c(5, 10, 2, 8),
#'   spell_abv_90p = c(2, 3, 1, 2),
#'   coverage_fraction = c(0.6, 0.4, 0.7, 0.3)
#' )
#' agg_to_adm_div(df)
#'

agg_to_adm_div <- function(df, match_col, extra_col = NULL) {
    df |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(c("ID_adm_div", "lag")))) |>
        dplyr::summarise(
            dplyr::across(
                .cols = {{extra_col}},
                .fns = unique),
            dplyr::across(
                .cols = dplyr::matches(match_col),
                .fns = ~ weighted.mean(.x, w = coverage_fraction, na.rm = TRUE)),
            .groups = "drop") |>
        dplyr::relocate({{extra_col}}, .after = ID_adm_div)
}
