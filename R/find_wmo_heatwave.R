#' Compute Heat Wave Spells (WMO Definition)
#'
#' Identifies heat wave spells according to the World Meteorological Organization (WMO) criteria:
#' a heat wave is defined as a sequence of consecutive days where the daily temperature
#' exceeds the long-term monthly average by at least a specified 5 degree Celsius.
#'
#'
#' @param df A data frame with an `ID` column and daily temperature columns
#'        (in wide format, named with date-like strings, e.g. `"2000-01-01"`).
#' @param excess Numeric. The minimum temperature anomaly above the monthly mean
#'        required to classify a day as "hot". Default is 5.
#' @param min_spell Integer. The minimum number of consecutive hot days to qualify
#'        as a heat wave spell. Default is 2.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item `ID`: identifier for the unit.
#'     \item `date`: the date of observation.
#'     \item `value`: the daily temperature.
#'     \item `spell_wmo`: an integer index identifying consecutive heat wave days (0 if none).
#'   }
#'
#' @details
#' For each `ID` and month, the monthly mean temperature is computed. A day is flagged
#' as part of a heat wave if its temperature exceeds the monthly mean by at least `excess`
#' and it belongs to a sequence of at least `min_spell` consecutive days.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   ID = 1,
#'   `2000-01-01` = 20,
#'   `2000-01-02` = 22,
#'   `2000-01-03` = 25,
#'   `2000-01-04` = 28
#' )
#' find_wmo_heatwave(df, excess = 5, min_spell = 2)
#' }
#'
#' @export

find_wmo_heatwave <- function(df, excess = 5L, min_spell = 2L) {
    df_long <- df |>
        dplyr::select(ID, dplyr::matches("[0-9]{4}")) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        dplyr::rename_with(to_date) |>
        data.table::as.data.table() |>
        data.table::melt(id.vars = "ID",
                         variable.name = "date",
                         value.name = "value")
    df_long[, month := substr(date, 6, 7), ]
    df_long[, avg := mean(value), by = .(ID, month)]
    df_long[, diff := (value - avg) >= excess]
    df_long[, spell_wmo := compute_spell(diff, min_spell)]
    df_long |>
        dplyr::select(-c(month, avg, diff))
}
