#' Filter data frame rows based on a date range
#'
#' This function filters rows in a data frame based on whether the `date` column
#' values fall within a specified start and end date range. The date range is
#' provided in a data frame with `start_date` and `end_date` columns.
#'
#' @param df A data frame containing a `date` column to be filtered.
#' @param dates A data frame or list with two columns: `start_date` and
#'   `end_date`, which define the range for filtering.
#'
#' @return A data frame containing only rows where the `date` column falls
#'   between the specified `start_date` and `end_date`.
#'
#' @export

filter_between <- function(df, dates) {
    df |>
        dplyr::filter(dplyr::between(date, dates$start_date, dates$end_date))
}
