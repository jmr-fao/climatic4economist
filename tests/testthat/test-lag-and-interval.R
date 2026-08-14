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

test_that("both modes count whole units toward zero", {
    # a span of -31.2 years is 31 whole years, not 32
    start <- as.Date("1992-10-14")
    end <- as.Date("2024-01-01")

    expect_equal(find_lag(start, end, unit = "year", calendar = TRUE), -31)
    expect_equal(find_lag(start, end, unit = "year", calendar = FALSE), -31)
})

test_that("the two modes agree on rounding direction for negative spans", {
    # calendar = TRUE truncated while calendar = FALSE floored, so the same
    # data could be binned one unit apart depending only on the mode
    set.seed(1)
    start <- as.Date("1990-01-01") + sample.int(12000, 500, replace = TRUE)
    end <- as.Date("2024-01-01")

    for (unit in c("year", "month")) {
        cal <- find_lag(start, end, unit = unit, calendar = TRUE)
        dur <- find_lag(start, end, unit = unit, calendar = FALSE)
        # they may differ by one where the unit lengths differ, never by more
        expect_lte(max(abs(cal - dur)), 1)
        # and never in opposite directions
        expect_true(all(sign(cal) == sign(dur) | cal == 0 | dur == 0))
    }
})

test_that("fixed-duration units keep their documented lengths", {
    # a duration year is 365.25 days and a duration month 30.4375 days
    ref <- as.Date("2024-01-01")
    expect_equal(find_lag(ref + 365, ref, unit = "year", calendar = FALSE), 0)
    expect_equal(find_lag(ref + 366, ref, unit = "year", calendar = FALSE), 1)
    expect_equal(find_lag(ref + 30, ref, unit = "month", calendar = FALSE), 0)
    expect_equal(find_lag(ref + 31, ref, unit = "month", calendar = FALSE), 1)
})

test_that("weeks and days are identical in both modes", {
    set.seed(2)
    start <- as.Date("2020-01-01") + sample.int(2000, 200, replace = TRUE)
    end <- as.Date("2024-01-01")

    for (unit in c("week", "day")) {
        expect_equal(find_lag(start, end, unit = unit, calendar = TRUE),
                     find_lag(start, end, unit = unit, calendar = FALSE))
    }
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

test_that("an incomplete final month is not counted", {
    # the calendar months differ by 12, but the span is three days short of a
    # year: reading the components alone would over-count by one
    expect_equal(find_lag(as.Date("1997-07-09"), as.Date("1996-07-12"),
                          unit = "month"), 11)
    expect_equal(find_lag(as.Date("1997-07-09"), as.Date("1996-07-12"),
                          unit = "year"), 0)
    # the same correction has to apply when the span runs backwards
    expect_equal(find_lag(as.Date("1996-07-12"), as.Date("1997-07-09"),
                          unit = "month"), -11)
})

test_that("a month elapses on the matching day of the month", {
    expect_equal(find_lag(as.Date("2024-03-11"), as.Date("2024-02-12"),
                          unit = "month"), 0)
    expect_equal(find_lag(as.Date("2024-03-12"), as.Date("2024-02-12"),
                          unit = "month"), 1)
})

test_that("month-end spans roll back rather than overshoot", {
    # 31 January to 28 February is a whole month, since February has no 31st
    expect_equal(find_lag(as.Date("2023-02-28"), as.Date("2023-01-31"),
                          unit = "month"), 1)
    # 29 February plus twelve months is 28 February, so that is a whole year
    expect_equal(find_lag(as.Date("2021-02-28"), as.Date("2020-02-29"),
                          unit = "year"), 1)
    expect_equal(find_lag(as.Date("2021-02-27"), as.Date("2020-02-29"),
                          unit = "year"), 0)
})

test_that("a year counts exactly as twelve months", {
    set.seed(5)
    start <- as.Date("1995-01-01") + sample.int(11000, 300, replace = TRUE)
    end <- as.Date("1995-01-01") + sample.int(11000, 300, replace = TRUE)

    expect_equal(find_lag(start, end, width = 1, unit = "year"),
                 find_lag(start, end, width = 12, unit = "month"))
    expect_equal(find_lag(start, end, width = 2, unit = "year"),
                 find_lag(start, end, width = 24, unit = "month"))
})

test_that("find_lag is vectorised over either argument", {
    starts <- as.Date(c("2024-06-01", "2023-06-01", "2022-06-01"))
    ends <- as.Date(c("2024-01-01", "2023-01-01", "2022-01-01"))

    expect_equal(find_lag(starts, as.Date("2024-01-01"), unit = "month"),
                 c(5, -7, -19))
    expect_equal(find_lag(as.Date("2024-06-01"), ends, unit = "month"),
                 c(5, 17, 29))
    expect_equal(find_lag(starts, ends, unit = "month"), c(5, 5, 5))
})

test_that("find_lag propagates NA and preserves empty input", {
    expect_true(is.na(find_lag(as.Date(NA), as.Date("2020-01-01"),
                               unit = "month")))
    expect_equal(find_lag(as.Date(c("2021-01-01", NA)), as.Date("2020-01-01"),
                          unit = "month"),
                 c(12, NA))
    expect_length(find_lag(as.Date(character()), as.Date("2020-01-01"),
                           unit = "month"), 0)
})
