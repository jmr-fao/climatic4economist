#' Compute spell percentiles and summary statistics
#'
#' This function calculates dry spell statistics, including mean, standard deviation,
#' and selected percentiles, for each month within a given dataset.
#'
#' @param df A data frame containing precipitation data with an `ID` column
#'           and daily precipitation values.
#' @param p A numeric vector of probabilities (e.g., `c(0.1, 0.5, 0.9)`)
#'          representing the percentiles to compute.
#' @param dry_treshold A numeric value (default = 0.1) defining the precipitation
#'                     threshold below which a day is considered dry.
#'
#' @return A data frame containing:
#' - `dry_day_mean`: Average proportion of dry days in each month.
#' - `dry_spell_mean`: Mean duration of dry spells per month.
#' - `dry_spell_sd`: Standard deviation of dry spell duration.
#' - `dry_spell_Xp`: Dry spell duration at the specified percentile `Xp`.
#'
#' @details
#' The function follows these steps:
#' 1. Reshapes `df` from wide to long format, extracting date and precipitation values.
#' 2. Converts the `date` column into a proper date object and extracts the `month`.
#' 3. Identifies dry days based on `dry_treshold` and computes dry spell durations.
#' 4. Calculates monthly summary statistics (`mean`, `sd`) for dry spells.
#' 5. Computes dry spell percentiles as specified by `p`.
#' 6. Merges all computed metrics into a single output table.
#'
#' @export
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   ID = rep(1:3, each = 10),
#'   `2000` = runif(30, 0, 5),
#'   `2001` = runif(30, 0, 5),
#'   `2002` = runif(30, 0, 5)
#' )
#'
#' calc_pct_spell(df, p = c(0.1, 0.5, 0.9))

calc_pct_spell <- function(extr_day, p, min_spell = 2) {
    find_spell(extr_day, min_spell = min_spell) |>
        dplyr::mutate(month = substr(date, 6, 7),
                      .after = date) |>
        dplyr::group_by(ID, month) |>
        dplyr::reframe(
            dplyr::across(
                .cols = dplyr::matches("spell"),
                .fns = ~quantile_df(.x, p),
                .unpack = TRUE)) |>
        dplyr::select(ID, month, dplyr::matches("[0-9]?[0-9]p$")) |>
        dplyr::mutate(dplyr::across(
            .cols = dplyr::matches("spell"),
            .fns = as.integer))
}
