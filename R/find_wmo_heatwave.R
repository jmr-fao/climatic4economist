#' Compute Heat Wave Spells (WMO Definition)
#'
#' Identifies heat wave spells according to the World Meteorological Organization (WMO) criteria:
#' a heat wave is defined as a sequence of consecutive days where the daily temperature
#' exceeds the long-term monthly average by at least a specified 5 degree Celsius.
#'
#'
#' @param df A data frame with a unit identifier column and daily temperature
#'        columns (in wide format, named with date-like strings, e.g.
#'        `"2000-01-01"`).
#' @param excess Numeric. The minimum temperature anomaly above the monthly mean
#'        required to classify a day as "hot". Default is 5.
#' @param min_spell Integer. The minimum number of consecutive hot days to qualify
#'        as a heat wave spell. Default is 2.
#' @param id Optional character string naming the column that identifies the
#'   units. When `NULL`, `ID` is used, or `ID_adm_div` when that is the only
#'   one present.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item the identifier column, carried through under its own name.
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

find_wmo_heatwave <- function(df, excess = 5L, min_spell = 2L, id = NULL) {
    key <- resolve_key(df, id)

    df_long <- df |>
        dplyr::select(dplyr::all_of(key), dplyr::matches("[0-9]{4}")) |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(key)), .keep_all = TRUE) |>
        # to_date() rewrites `_` and `.` and strips a leading X, which would
        # mangle a key such as ID_adm_div; only the date columns need it
        dplyr::rename_with(to_date, .cols = -dplyr::all_of(key)) |>
        data.table::as.data.table() |>
        data.table::melt(id.vars = key,
                         variable.name = "date",
                         value.name = "value")

    # spells depend on row order, so parse the factor labels and sort by date
    df_long[, date := parse_date_label(date)]
    data.table::setorderv(df_long, c(key, "date"))
    df_long[, month := month_label(date), ]
    df_long[, avg := mean(value), by = c(key, "month")]
    df_long[, diff := (value - avg) >= excess]
    df_long[, spell_wmo := compute_spell(diff, min_spell)]
    df_long |>
        dplyr::select(-c(month, avg, diff))
}
