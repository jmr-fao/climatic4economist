test_that("find_lag counts complete calendar intervals", {
    expect_equal(find_lag(as.Date("2025-01-01"), as.Date("2020-01-01")), 5)
    expect_equal(
        find_lag(as.Date("2023-01-01"), as.Date("2020-01-01"),
                 width = 6, unit = "month"),
        6
    )
})

test_that("find_lag returns 0 for a date inside the first interval", {
    expect_equal(find_lag(as.Date("2024-06-30"), as.Date("2024-01-01"),
                          unit = "year"), 0)
})

test_that("find_lag supports fixed-duration intervals", {
    # 2020-01-01 to 2023-01-01 spans 1096 days: 3 calendar years, and
    # 1096 / 365.25 = 3.0007 fixed-duration years
    expect_equal(find_lag(as.Date("2023-01-01"), as.Date("2020-01-01"),
                          unit = "year", calendar = TRUE), 3)
    expect_equal(find_lag(as.Date("2023-01-01"), as.Date("2020-01-01"),
                          unit = "year", calendar = FALSE), 3)
})

test_that("calendar and fixed-duration modes disagree near a boundary", {
    # 2021-01-01 to 2022-01-01 is 365 days: a full calendar year, but short of
    # the 365.25-day fixed-duration year
    expect_equal(find_lag(as.Date("2022-01-01"), as.Date("2021-01-01"),
                          unit = "year", calendar = TRUE), 1)
    expect_equal(find_lag(as.Date("2022-01-01"), as.Date("2021-01-01"),
                          unit = "year", calendar = FALSE), 0)
})

test_that("find_lag handles weeks and days", {
    expect_equal(find_lag(as.Date("2024-01-15"), as.Date("2024-01-01"),
                          unit = "day"), 14)
    expect_equal(find_lag(as.Date("2024-01-15"), as.Date("2024-01-01"),
                          unit = "week"), 2)
})

test_that("find_lag is vectorised over the start date", {
    dates <- as.Date(c("2024-01-01", "2023-01-01", "2022-01-01"))
    expect_equal(find_lag(dates, as.Date("2024-01-01"), unit = "year"),
                 c(0, -1, -2))
})

# --- filter_by_interview -----------------------------------------------------

test_that("filter_by_interview keeps observations inside the window", {
    df <- data.frame(
        interview = as.Date("2024-01-10"),
        date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-08", "2024-01-09"))
    )
    out <- filter_by_interview(df, interview = interview, interval = "3 day")
    expect_equal(out$date, as.Date(c("2024-01-08", "2024-01-09")))
})

test_that("filter_by_interview accepts plural interval units", {
    # regression: "3 months" used to fail match.arg because the trailing "s"
    # was not stripped, unlike in extr_day_index()
    df <- data.frame(
        interview = as.Date("2024-01-10"),
        date = as.Date(c("2024-01-01", "2024-01-08"))
    )
    for (unit in c("day", "days", "week", "weeks",
                   "month", "months", "year", "years")) {
        expect_no_error(
            filter_by_interview(df, interview = interview,
                                interval = paste("1", unit))
        )
    }
})

test_that("filter_by_interview shifts by whole days, not by subtraction", {
    # regression: the "day" branch computed add_days(date - n) rather than
    # add_days(date, -n), so the window was wrong
    df <- data.frame(
        interview = as.Date("2024-01-10"),
        date = as.Date(c("2024-01-06", "2024-01-07", "2024-01-08"))
    )
    out <- filter_by_interview(df, interview = interview, interval = "3 days")
    # window is (2024-01-07, 2024-01-10]
    expect_equal(out$date, as.Date(c("2024-01-08")))
})

test_that("filter_by_interview drops rows with a missing interview date", {
    df <- data.frame(
        interview = as.Date(c("2024-01-10", NA)),
        date = as.Date(c("2024-01-09", "2024-01-09"))
    )
    out <- expect_output(
        filter_by_interview(df, interview = interview, interval = "1 year"),
        "Missing interview"
    )
    expect_equal(nrow(out), 1L)
})

test_that("filter_by_interview rejects an unknown interval unit", {
    df <- data.frame(interview = as.Date("2024-01-10"),
                     date = as.Date("2024-01-09"))
    expect_error(
        filter_by_interview(df, interview = interview, interval = "1 fortnight")
    )
})
