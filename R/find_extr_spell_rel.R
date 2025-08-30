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

find_extr_spell_rel <- function(spell, threshold, min_spell = 2) {
    # Find extreme thresholds used to compute the spell for the percentiles
    u_perc_thresh_values <- threshold |>
        dplyr::select(dplyr::matches("abv")) |>
        names() |>
        gsub("(^.*)_([0-9]*\\.?[0-9]?)_[0-9]{2}p$", "\\2", x = _) |>
        as.numeric() |>
        unique()

    l_perc_thresh_values <- threshold |>
        dplyr::select(dplyr::matches("blw")) |>
        names() |>
        gsub("(^.*)_([0-9]*\\.?[0-9]?)_[0-9]{2}p$", "\\2", x = _) |>
        as.numeric() |>
        unique()

    # Match the extreme threshold used for computing the spell for percentiles
    # and the observed spell
    if(length(u_perc_thresh_values) == 0) {
        u_perc_thresh_values <- NULL
    } else {
        u_spell_thresh_values <- spell |>
            dplyr::select(dplyr::matches("abv")) |>
            names() |>
            gsub("(^.*)_([0-9]*\\.?[0-9]?)$", "\\2", x = _) |>
            as.numeric() |>
            unique()
        u_perc_thresh_values <- intersect(u_spell_thresh_values, u_perc_thresh_values)
    }
    if(length(l_perc_thresh_values) == 0) {
        l_perc_thresh_values <- NULL
    } else {
        l_spell_thresh_values <- spell |>
            dplyr::select(dplyr::matches("blw")) |>
            names() |>
            gsub("(^.*)_([0-9]*\\.?[0-9]?)$", "\\2", x = _) |>
            as.numeric() |>
            unique()
        l_perc_thresh_values <- intersect(l_spell_thresh_values, l_perc_thresh_values)
    }

    # Print the extreme threshold that matches the computation of the observed spell
    # and of the percentile spell
    if (length(l_perc_thresh_values) != 0) {
        cat("Finding extreme spells for spell computed below", l_perc_thresh_values, "\n")
    }
    if (length(l_perc_thresh_values) != 0) {
        cat("Finding extreme spells for spell computed above", u_perc_thresh_values, "\n")
    }
    # If there is no match stop execution
    if(length(l_perc_thresh_values) == 0 & length(l_perc_thresh_values) == 0) {
        stop(paste0("Error: the spell and the treshold do not match!",
                    "They are computed over different extreme values."))
    }

    df_full <- dplyr::full_join(spell, threshold, by = c("ID", "month"))

    purrr::map(c(l_perc_thresh_values, u_perc_thresh_values),
               \(x) df_full |>
                   dplyr::select(
                       ID, date, month,
                       dplyr::matches(
                           # get the specific spell and its threshold
                           paste0("spell_[a-z]{3}_", x, "$", "|",
                                  "spell_[a-z]{3}_", x, "_"))) |>
                   dplyr::rename_with(.cols = dplyr::matches("[0-9]$"),
                                      .fn = ~gsub(".*", "spell", .x)) |>
                   dplyr::mutate(
                       dplyr::across(
                           .cols = dplyr::matches("[0-9]{2}p$"),
                           # return excess days from threshold
                           .fns = ~dplyr::if_else(spell < .x | is.na(spell),
                                                  NA_integer_, spell - .x))) |>
                   dplyr::select(ID, date, month, dplyr::matches("[0-9]{2}p$"))) |>
        purrr::reduce(dplyr::full_join, c("ID", "date", "month"))
}
