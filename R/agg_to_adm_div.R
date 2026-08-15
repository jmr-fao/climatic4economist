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
#' @param id Optional character string naming the column to aggregate by. When
#'   `NULL`, `ID_adm_div` is used, falling back to `ID`. Note the preference is
#'   the reverse of the rest of the package: a table carrying both should
#'   aggregate to the administrative division, which is what this function is
#'   for. Set `id` to group by anything else.
#'
#' @return A data frame grouped by the identifier column and `lag`, with values
#'   aggregated by a weighted mean using `coverage_fraction` as weights.
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

agg_to_adm_div <- function(df, match_col, extra_col = NULL, id = NULL) {
    # Candidates are reversed here: the function aggregates *to* administrative
    # divisions, so a table carrying both keys should group by ID_adm_div.
    # Resolving up front also replaces what used to be an opaque tidyselect
    # error thrown later by relocate() when the column was absent.
    key <- resolve_key(df, id, candidates = c("ID_adm_div", "ID"))

    df |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(c(key, "lag")))) |>
        dplyr::summarise(
            dplyr::across(
                .cols = {{extra_col}},
                .fns = unique),
            dplyr::across(
                .cols = dplyr::matches(match_col),
                .fns = ~ weighted.mean(.x, w = coverage_fraction, na.rm = TRUE)),
            .groups = "drop") |>
        dplyr::relocate({{extra_col}}, .after = dplyr::all_of(key))
}
