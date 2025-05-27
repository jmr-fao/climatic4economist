#' Identify relative extreme spells
#'
#' This function detects extreme spells by comparing observed spell
#' duration with predefined percentile-based thresholds.
#'
#' @param df A data frame containing precipitation data with an `ID` column and
#'   daily precipitation values .
#' @param threshold A data frame containing dry spell percentile thresholds for
#'   each `ID` and `month`, typically computed using `calc_pct_spell()`.
#' @param dry_treshold A numeric value (default = 0.1) defining the
#'   precipitation threshold below which a day is considered dry.
#'
#' @return A data frame containing:
#' - `ID`: Unique identifier for locations or individuals.
#' - `date`: The date of the observation.
#' - `dry_spell_abv_Xp`: Difference between observed dry spell duration and
#'   threshold `Xp`, where `Xp` represents different percentiles.
#'
#' @export

find_extr_spell_rel <- function(df, threshold, min_spell = 2) {

    u_thresh_values <- threshold |>
        dplyr::select(dplyr::matches("abv")) |>
        names() |>
        gsub("(^.*)_([0-9]*\\.?[0-9]?)_[0-9]{2}p$", "\\2", x = _) |>
        as.numeric() |>
        unique()

    l_thresh_values <- threshold |>
        dplyr::select(dplyr::matches("blw")) |>
        names() |>
        gsub("(^.*)_([0-9]*\\.?[0-9]?)_[0-9]{2}p$", "\\2", x = _) |>
        as.numeric() |>
        unique()

    if(length(u_thresh_values) == 0) u_thresh_values <- NULL
    if(length(l_thresh_values) == 0) l_thresh_values <- NULL

    spell <- find_extr_abs_spell(df,
                                 u_thresh = u_thresh_values,
                                 l_thresh = l_thresh_values,
                                 min_spell = min_spell) |>
        dplyr::mutate(month = substr(date, 6, 7),
                      .after = date)

    df_full <- dplyr::full_join(spell, threshold, by = c("ID", "month"))

    purrr::map(c(l_thresh_values, u_thresh_values),
               \(x) df_full |>
                   dplyr::select(
                       ID, date, month,
                       dplyr::matches(
                           paste0("spell_[a-z]{3}_", x, "$", "|", # get the specific spell and its threshold
                                  "spell_[a-z]{3}_", x, "_"))) |>
                   dplyr::rename_with(.cols = dplyr::matches("[0-9]$"),
                                      .fn = ~gsub(".*", "spell", .x)) |>
                   dplyr::mutate(
                       dplyr::across(
                           .cols = dplyr::matches("[0-9]{2}p$"),
                           .fns = ~dplyr::if_else(spell < .x | is.na(spell),
                                                  NA_integer_, spell - .x))) |>
                   dplyr::select(ID, date, month, dplyr::matches("[0-9]{2}p$"))) |>
        purrr::reduce(dplyr::full_join, c("ID", "date", "month"))
}
