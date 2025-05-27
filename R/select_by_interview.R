#' Select time observations based on interview date and time interval
#'
#' This function filters a dataframe based on an interview date and a specified
#' time interval. It reshapes the data accordingly, either in long or wide
#' format.
#'
#' @param df A dataframe containing time-series data.
#' @param interview <data-masking>. The column indicating the interview date.
#' @param interval A string specifying the time range for selection (e.g., `"2
#'   years"`, `"6 months"`).
#' @param wide Logical. A logical value indicating whether to return data in
#'   wide format (default is `FALSE`).
#'
#' @return A dataframe filtered based on the specified time interval, returned
#'   in either long or wide format.
#'
#' @export
#'
#' @examples
#' df <- data.frame(interview_date = c("2023-01-15", "2022-06-20"),
#'                  X2022_12_02 = c(100, 200),
#'                  X2022_06_21 = c(150, 250))
#'
#' select_by_interview(df, interview_date, "1 year")

select_by_interview <- function(df, interview, interval, wide = FALSE) {

    df_long <- df |>
        tidyr::pivot_longer(cols = dplyr::matches("[0-9]{4}"),
                            names_to = "date")

    df_filtered <- filter_by_interview(df = df_long,
                                       interview = {{interview}},
                                       interval = interval,
                                       missing = "drop")
    if (wide) {
        df_filtered |>
            tidyr::pivot_wider(names_from = date,
                               values_from = value) |>
            dplyr::rename_with(~gsub("\\-", "_", .x)) |>
            dplyr::rename_with(.cols = dplyr::matches("[0-9]{4}"),
                               ~paste0("X", .x))
    } else {
        df_filtered
    }
}
