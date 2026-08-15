#' Select time observations based on interview date and time interval
#'
#' This function filters a dataframe based on an interview date and a specified
#' time interval. It can accept an existing column name,
#' a literal character string, or a Date object for the interview date.
#'
#' @param df A dataframe containing longitudinal data with date-based columns
#'   (identified by a 4-digit year pattern).
#' @param interview <[`data-masking`][rlang::args_data_masking]>. Either an
#'   existing column name containing dates, or a single value (Date or character)
#'   to be applied to all rows.
#' @param interval A string specifying the time range for selection (e.g.,
#'   `"2 years"`, `"6 months"`). Passed to `filter_by_interview`.
#' @param wide Logical. If `TRUE`, returns the data in wide format. If `FALSE`
#'   (default), returns long format.
#'
#' @details
#' If `interview` is not a column name found in `df`, the function creates a
#' temporary reference column using the value provided. Rows with missing
#' interview dates are automatically dropped, and a message is displayed
#' indicating the count of dropped rows.
#'
#' The function identifies data columns to pivot by searching for 4-digit
#' years (e.g., "2023", "X2023_01_01") in the column names.
#'
#' @return A filtered tibble/dataframe. If `wide = TRUE`, the columns matching
#'   the date pattern are restored to the wide headers.
#'
#' @export
#'
#' @examples
#' # Usage with a column name:
#' df <- data.frame(
#'   id = 1:2,
#'   interview_date = as.Date(c("2023-01-15", "2022-06-20")),
#'   X2022_12_02 = c(100, 200),
#'   X2022_06_21 = c(150, 250)
#' )
#' select_by_interview(df, interview_date, "1 year")
#'
#' # Usage with a literal date:
#' select_by_interview(df, "2023-01-01", "6 months", wide = TRUE)

select_by_interview <- function(df, interview, interval, wide = FALSE) {

    # 1) Check if column exist or not
    int_enquo <- rlang::enquo(interview)
    is_col_input <- rlang::as_label(int_enquo) %in% names(df)

    if (is_col_input) {
        # It's an existing column
        interview_var <- int_enquo
    } else {
        # it's not an existing column name, create a temporary one
        df <- df |>
            dplyr::mutate(temp_interview_col = !!int_enquo,
                          .before = 1)
        interview_var <- rlang::sym("temp_interview_col")
    }

    # 2) drop missing date of interview
    df_cln <- df |>
        dplyr::filter(!is.na(!!interview_var))

    n_dropped <- nrow(df) - nrow(df_cln)
    if (n_dropped > 0) {
        message(paste(n_dropped, "rows with missing interview dates were dropped."))
    }

    # 3) to long format
    df_long <- df_cln |>
        tidyr::pivot_longer(cols = dplyr::matches(date_pattern()),
                            names_to = "date")

    # 4) filter based on interval
    df_filtered <- filter_by_interview(df = df_long,
                                       interview = {{interview_var}},
                                       interval = interval,
                                       missing = "skip")

    # 5) Clean up the temporary column
    if (!is_col_input) {
        df_filtered <- df_filtered |>
            dplyr::select(-temp_interview_col)
    }

    # 6) wide format
    if (wide) {
        df_filtered |>
            tidyr::pivot_wider(names_from = date,
                               values_from = value)
    } else {
        df_filtered
    }
}
