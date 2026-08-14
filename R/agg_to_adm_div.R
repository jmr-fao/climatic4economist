#' Aggregate Data to Administrative Divisions
#'
#' This function aggregates daily and spell-based extreme event indicators to the level
#' of administrative divisions using a weighted mean, where weights are based on the
#' `coverage_fraction` of each observation.
#'
#' @param df A data frame containing extreme event indicators, administrative division IDs,
#'   and coverage fractions.
#' @param match_col A regular expression selecting the indicator columns to
#'   aggregate, for example `"^day|^spell"`.
#' @param extra_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional columns
#'   carried through unchanged. They must be constant within each group, since
#'   they are reduced with `unique()`.
#'
#' @return A data frame grouped by administrative division (`ID_adm_div`) and `lag`,
#'   with values aggregated by a weighted mean using `coverage_fraction` as weights.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID_adm_div = c(1, 1, 2, 2),
#'   lag = c(0, 0, 0, 0),
#'   day_abv_90p = c(5, 10, 2, 8),
#'   spell_abv_90p = c(2, 3, 1, 2),
#'   coverage_fraction = c(0.6, 0.4, 0.7, 0.3)
#' )
#' agg_to_adm_div(df, match_col = "^day|^spell")
#'

agg_to_adm_div <- function(df, match_col, extra_col = NULL) {
    # The key is fixed here, unlike elsewhere in the package: this function
    # aggregates *to* administrative divisions, so `ID_adm_div` is what it
    # means rather than whichever key the data happens to carry. Say so up
    # front, or `any_of()` below silently drops the grouping and the failure
    # surfaces later as an opaque tidyselect error from `relocate()`.
    if (!"ID_adm_div" %in% names(df)) {
        stop("`df` must contain an `ID_adm_div` column to aggregate by. ",
             "Administrative divisions come from `read_GAUL()` or ",
             "`read_geoBoundaries()`.", call. = FALSE)
    }

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
