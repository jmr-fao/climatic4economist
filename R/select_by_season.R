#' Select Columns of a Data Frame Corresponding to a Seasonal Period
#'
#' This function extracts only the columns of a data frame that fall within a
#' user-specified seasonal range. The data frame is assumed to contain identifier
#' columns (non-date) and time-series columns whose names contain years (e.g.,
#' "2021-04-01", "2022.07.15").
#'
#' @param df A `data.frame` or `tibble` with identifier columns and date-labeled
#'   columns.
#' @param start_month The starting month of the seasonal window. Can be provided
#'   as an integer (1–12), a month abbreviation (e.g., `"Apr"`), or a full month
#'   name (e.g., `"April"`).
#' @param end_month The ending month of the seasonal window. Same format as
#'   `start_month`.
#'
#' @details
#' If `start_month <= end_month`, the function returns all columns between those
#' two months (inclusive). If `start_month > end_month`, the function assumes the
#' season crosses the year boundary (e.g., November–February).
#'
#' Identifier columns are automatically preserved. Seasonal selection is applied
#' only to columns matching a date pattern (containing a 4-digit year).
#'
#' @return A `data.frame` containing identifier columns and date columns that
#'   fall within the specified seasonal period.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   ID = 1,
#'   "2021-01-01" = 10,
#'   "2021-04-01" = 15,
#'   "2021-07-01" = 20,
#'   "2021-11-01" = 25
#' )
#'
#' # Select April–July
#' select_by_season(df, "Apr", "Jul")
#'
#' # Select November–February (crosses year boundary)
#' select_by_season(df, 11, 2)
#' }
#'
#' @export

select_by_season <- function(df, start_month, end_month) {

    month_to_num <- function(m) {
        if (is.character(m)) {
            m <- tolower(m)
            # Match against month abbreviations or full names
            ifelse(!is.na(match(m, tolower(month.abb))),
                   match(m, tolower(month.abb)),
                   match(m, tolower(month.name)))
        } else {
            as.integer(m)
        }
    }

    col_names <- names(df)
    id_col <- col_names[!grepl("[0-9]{4}", col_names)]
    date_col <- col_names[grepl("[0-9]{4}", col_names)]
    month_col <- to_date(date_col) |>
        lubridate::ymd(truncated = 2) |>
        lubridate::month()

    sm <- month_to_num(start_month)
    em <- month_to_num(end_month)

    if (sm <= em) {
        keep <- month_col >= sm & month_col <= em
    } else {
        keep <- month_col >= sm | month_col <= em
    }

    date_between <- date_col[keep]
    df |>
        dplyr::select(dplyr::all_of(id_col), dplyr::all_of(date_between))
}
