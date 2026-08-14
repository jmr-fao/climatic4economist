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

filter_by_interview <- function(df, interview, interval, missing = "drop") {

    n_period <- as.numeric(gsub("[a-zA-Z]|\\s", "", interval))
    period <- tolower(gsub("[0-9]|\\s|s$", "", interval))
    # validate period
    period <- match.arg(tolower(period), c("year", "month", "week", "day"))


    interview_date <- clock::date_parse(to_date(dplyr::pull(df, {{interview}})))
    obs_date <- clock::date_parse(to_date(df$date))

    # Drop missing if requested
    if (missing == "drop") {
        valid <- !is.na(interview_date)
        if (any(!valid)) cat("Missing interview are dropped!\n")
        df <- df[valid, , drop = FALSE]
        interview_date <- interview_date[valid]
        obs_date <- obs_date[valid]
    }

    # Compute start_date in a vectorized way
    start_date <- switch(
        period,
        "year"  = clock::add_years(interview_date,  -n_period, invalid = "previous"),
        "month" = clock::add_months(interview_date, -n_period, invalid = "previous"),
        "week"  = clock::add_weeks(interview_date,  -n_period, invalid = "previous"),
        "day"   = clock::add_days(interview_date, -n_period)
    )

    keep <- obs_date > start_date & obs_date <= interview_date

    df[keep, , drop = FALSE]
}


