#' Compute Spells of Extreme Values
#'
#' This function identifies consecutive occurrences (spells) of extreme values
#' in a dataset, given a minimum spell length. It computes both above-threshold
#' and below-threshold spells.
#'
#' @param extr_day A data frame containing daily extreme values, with columns:
#'   - `ID`: Unique identifier for each entity.
#'   - `date`: The date of observation.
#'   - `value`: The observed value.
#'   - `day_abv_*`: Binary indicators for exceeding upper thresholds.
#'   - `day_blw_*`: Binary indicators for falling below lower thresholds.
#' @param min_spell Integer. The minimum number of consecutive days required to form a spell. Default is 2.
#'
#' @return A data frame with:
#'   - `ID`, `date`, `value`, and optionally `month` from `extr_day`.
#'   - Computed spell durations for both above-threshold and below-threshold events.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 1, 2, 2),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-01", "2024-01-02")),
#'   value = c(10, 12, 15, 8, 7),
#'   day_abv_90p = c(0, 1, 1, 0, 0),
#'   day_blw_10p = c(1, 0, 0, 1, 1)
#' )
#' find_spell(df, min_spell = 2)

find_spell <- function(extr_day, min_spell = 2) {
    spell_abv <- extr_day |>
        dplyr::group_by(ID) |>
        dplyr::arrange(date, .by_group = TRUE) |>
        dplyr::transmute(
            dplyr::across(.cols = dplyr::matches("^day_abv_\\-?[0-9]{1,2}\\.?[0-9]?p?$"),
                          .fns = ~compute_spell(.x, threshold = min_spell))) |>
        dplyr::rename_with(.fn = ~ gsub("^day", "spell", .x)) |>
        dplyr::ungroup() |>
        dplyr::select(-ID)

    spell_blw <- extr_day |>
        dplyr::group_by(ID) |>
        dplyr::arrange(date, .by_group = TRUE) |>
        dplyr::transmute(
            dplyr::across(.cols = dplyr::matches("^day_blw_\\-?[0-9]{1,2}\\.?[0-9]?p?$"),
                          .fns = ~compute_spell(.x, threshold = min_spell))) |>
        dplyr::rename_with(.fn = ~ gsub("^day", "spell", .x)) |>
        dplyr::ungroup() |>
        dplyr::select(-ID)

    extr_day |>
        dplyr::select(ID, date, value) |>
        list(spell_abv, spell_blw) |>
        purrr::compact() |>
        dplyr::bind_cols()
}
