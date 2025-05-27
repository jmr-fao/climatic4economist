#' Select time observations based on date range
#'
#' This function selects columns based on a specified date range.
#'
#' @param df A data frame with time-series data in wide format.
#' @param from character. A string representing the start date (optional).
#' @param to character. A string representing the end date (optional).
#'
#' @return A data frame filtered based on the specified date range.
#'
#' @details
#' The function requires both or just one between the starting date,
#' `from`, and the end date `to`. If both are provide the the function select
#' between the two dates. If only `from` is provided the function selects all
#' date after. If only `to` is provided the function selects all date
#' before. If none are provided returns the data frame unchanged.
#'
#' @export
#'
#' @examples
#' df <- data.frame(X2022.12 = c(100, 200),
#'                  X2022.06 = c(150, 250))
#'
#' select_by_dates(df, from = "2022-06-01", to = "2022-12-31")

select_by_dates <- function(df, from = NULL, to = NULL) {
    col_names <- names(df)
    id_col <- col_names[!grepl("[0-9]{4}", col_names)]
    date_col <- col_names[grepl("[0-9]{4}", col_names)]
    clock_col <- to_date(date_col) |>
        substr(1, 10) |>
        lubridate::ymd(truncated = 2)

    date_after <- 1:length(date_col)
    date_before <- 1:length(date_col)
    if(!is.null(from)) {
        date_after <- which(clock_col >= lubridate::ymd(from, truncated = 2))
    }
    if(!is.null(to)) {
        date_before <- which(clock_col <= lubridate::ymd(to, truncated = 2))
    }
    date_between <- date_col[intersect(date_after, date_before)]

    df |>
        dplyr::select(dplyr::all_of(id_col), dplyr::all_of(date_between))
}
