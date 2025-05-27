#' Filter data based on interview dates and a specified time interval
#'
#' This function filters rows in a data frame based on interview dates, which
#' are provided as a column in the data frame. The user can specify a time
#' interval in years or months, and the data will be filtered based on whether
#' the interview dates fall within the given period. Missing interview data can
#' either be dropped or retained.
#'
#' @param df A data frame containing the data to be filtered.
#' @param interview A column name in `df` representing the interview dates.
#' @param interval A string indicating the time interval, e.g., "1 year" or "3
#'   months".
#' @param missing A character string specifying what to do with missing
#'   interview data. Defaults to "drop", which removes rows with missing
#'   interview dates. Alternatively, can retain missing data by setting to
#'   "retain".
#'
#' @return A filtered data frame where rows are selected based on the interview
#'   dates falling within the specified interval.
#'
#' @export
#'
#' @examples
#' # Example data frame with interview dates
#' df <- data.frame(id = 1:5,
#'                  date = as.Date(c('2022-01-01', '2022-02-01',
#'                   '2022-03-01', NA, '2022-05-01')),
#'                  value = c(10, 20, 30, 40, 50))
#'
#' # Define interview and filter by 1 year interval
#' filter_by_interview(df, interview = "date", interval = "1 year")
#'
#' # Define interview and filter by 3 months interval
#' filter_by_interview(df, interview = "date", interval = "3 months")

# filter_by_interview <- function(df, interview, interval, missing = "drop") {
#
#     if (!grepl("^[0-9] ([Yy]ear|[Mm]onth)s?$", interval)) {
#         stop("Invalid interval format. Use '# years' or '# months'.")
#     }
#
#     n_period <- gsub("[a-zA-Z]| ", "", interval) |>
#         as.numeric()
#     period <- gsub("[0-9]| ", "", interval)
#
#     df <- df |>
#         dplyr::mutate(end_date = clock::date_parse(to_date(({{interview}}))))
#
#
#     if (missing == "drop") {
#         any_na <- dplyr::pull(df, end_date) |>
#             is.na() |>
#             any()
#         if (any_na) {
#             cat("Missing interview are dropped!\n")
#             df_missing <- df |>
#                 dplyr::filter(!is.na(end_date))
#         } else {
#             df_missing <- df
#         }
#     }
#
#     df_clock <- df_missing |>
#         dplyr::mutate(date = clock::date_parse(as.character(to_date(date)))) |>
#         tidyr::nest(data = -end_date, .by = end_date) |>
#         dplyr::mutate(
#             start_date = dplyr::case_when(
#                 grepl("[Yy]ear", period) ~ clock::add_years(end_date,
#                                                             -n_period,
#                                                             invalid = "previous"),
#                 grepl("[Mm]onth", period) ~ clock::add_months(end_date,
#                                                               -n_period,
#                                                               invalid = "previous"),
#                 .default = NA),
#             .after = end_date)
#
#     df_clock  |>
#         tidyr::nest(dates = c(start_date, end_date)) |>
#         dplyr::mutate(
#             data_2 = purrr::map2(data, dates, filter_between)) |>
#         dplyr::select(-c(data, dates)) |>
#         tidyr::unnest(data_2)
# }
filter_by_interview <- function(df, interview, interval, missing = "drop") {
    if (!grepl("^[0-9]+\\s*([Yy]ear|[Mm]onth)s?$", interval)) {
        stop("Invalid interval format. Use '# years' or '# months'.")
    }

    n_period <- as.numeric(gsub("[a-zA-Z\\s]", "", interval))
    period <- tolower(gsub("[0-9\\s]", "", interval))

    # Precompute interview date only once
    interview_date <- clock::date_parse(to_date(dplyr::pull(df, {{interview}})))

    # Drop missing if requested
    if (missing == "drop") {
        valid <- !is.na(interview_date)
        if (any(!valid)) cat("Missing interview are dropped!\n")
        df <- df[valid, , drop = FALSE]
        interview_date <- interview_date[valid]
    }

    # Parse observation date once
    obs_date <- clock::date_parse(to_date(df$date))

    # Calculate start_date vectorized
    start_date <- if (grepl("year", period)) {
        clock::add_years(interview_date, -n_period, invalid = "previous")
    } else if (grepl("month", period)) {
        clock::add_months(interview_date, -n_period, invalid = "previous")
    } else {
        stop("Unsupported interval unit.")
    }

    # Vectorized filter
    keep <- obs_date >= start_date & obs_date <= interview_date

    # Return filtered data
    df[keep, , drop = FALSE]
}
