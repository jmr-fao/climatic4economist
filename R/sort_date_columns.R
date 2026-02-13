#' Sort date columns in a wide data frame chronologically
#'
#' Reorders columns in a data frame where some columns represent dates
#' encoded in the column names (e.g. `"XYYYY_MM_DD"`). Non-date columns are kept
#' in their original order, while date columns are sorted in
#' chronological order.
#'
#' This function is useful for panel-style wide datasets where time
#' variables are stored as separate columns, for example when reshaping
#' weather, raster, or other time-series data from long to wide format.
#'
#' @param df A data frame containing date columns named using the
#'   pattern `"XYYYY_MM_DD"`.
#'
#' @return A data frame with the same columns as \code{df}, where
#'   date columns are reordered chronologically.
#'
#' @details
#' The function identifies date columns using the regular expression
#' \code{"\\d{4}_\\d{2}_\\d{2}$"}. Columns not matching
#' this pattern are assumed to be identifier or metadata variables and
#' remain unchanged.
#'
#' The function does not modify column values or types.
#'
#' @examples
#' sort_date_columns(df)
#'
#' @export
sort_date_columns <- function(df) {

    date_cols <- names(df)[grepl("\\d{4}_\\d{2}_\\d{2}$", names(df))]

    date_cols_sorted <- date_cols |>
        (\(x) x[order(as.Date(sub("^X", "", x), "%Y_%m_%d"))])()

    df |>
        dplyr::select(
            -dplyr::all_of(date_cols),
            dplyr::all_of(date_cols_sorted)
        )
}
