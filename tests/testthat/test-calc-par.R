# Month-end dates for two full years. Built from month starts and rolled to the
# end of each month, because seq.Date(by = "month") from a 31st overflows
# (2022-01-31 + 1 month lands on 2022-03-03), which would silently give a
# fixture spanning fewer months than it appears to.
month_end_dates <- function(from = "2022-01-01", to = "2023-12-01") {
    starts <- seq(as.Date(from), as.Date(to), by = "month")
    format(lubridate::ceiling_date(starts, "month") - 1, "%Y-%m-%d")
}

# Two locations, values constant per ID so the expected statistics are obvious.
monthly_df <- function(values = c(1, 10), ...) {
    dates <- month_end_dates(...)
    df <- data.frame(ID = c("1", "2"))
    for (d in dates) df[[d]] <- values
    df
}

test_that("the fixture spans one observation per distinct month", {
    dates <- month_end_dates()
    expect_length(dates, 24L)
    expect_length(unique(substr(dates, 1, 7)), 24L)
    expect_equal(dates[1], "2022-01-31")
    expect_equal(dates[24], "2023-12-31")
})

test_that("calc_par computes a statistic per spatial unit", {
    out <- calc_par(monthly_df(), pars = list(avg = mean))

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_named(out, c("ID", "avg"))
    expect_equal(out$avg, c(1, 10))
})

test_that("calc_par accepts several statistics at once", {
    out <- calc_par(monthly_df(), pars = list(avg = mean, total = sum))

    expect_named(out, c("ID", "avg", "total"))
    # 24 monthly observations per ID
    expect_equal(out$total, c(24, 240))
})

test_that("calc_par applies a prefix and a suffix to the statistic columns", {
    out <- calc_par(monthly_df(), pars = list(avg = mean),
                    prefix = "pre", suffix = "10yrs")

    expect_named(out, c("ID", "pre_avg_10yrs"))
    # identifier columns are never renamed
    expect_true("ID" %in% names(out))
})

test_that("calc_par keeps every identifier column it recognises", {
    df <- monthly_df()
    df$x_cell <- c(10, 20)
    df$y_cell <- c(30, 40)

    out <- calc_par(df, pars = list(avg = mean))
    expect_true(all(c("ID", "x_cell", "y_cell") %in% names(out)))
})

test_that("calc_par averages yearly totals when agg_period is 'year'", {
    # 12 months of 1 per year -> yearly total 12, averaged over 2 years -> 12
    out <- calc_par(monthly_df(), pars = list(total = sum), agg_period = "year")

    expect_equal(nrow(out), 2L)
    expect_equal(out$total, c(12, 120))
})

test_that("calc_par rejects an unsupported agg_period", {
    expect_error(calc_par(monthly_df(), pars = list(avg = mean),
                          agg_period = "decade"),
                 "Unsupported")
})

test_that("calc_par warns when the last date is not a December month end", {
    df <- monthly_df(values = 1, from = "2022-01-01", to = "2023-06-01")

    expect_warning(calc_par(df, pars = list(total = sum), agg_period = "year"),
                   "not the last day of a month")
})

test_that("month aggregation returns no rows at all for monthly input", {
    # aggregate_by_month() filters on n_days > 1. Data already at monthly
    # resolution has exactly one observation per period, so every period is
    # dropped and the caller silently loses every unit: no error, no warning,
    # and an empty frame rather than a missing value.
    df <- monthly_df(values = 1)
    out <- calc_par(df, pars = list(total = sum), agg_period = "month")

    expect_equal(nrow(out), 0L)
    expect_true("ID" %in% names(out))
})

test_that("the n_days > 1 filter turns on whether a partial month has 1 or 2 days", {
    # A full December alongside a November represented by a single day: the
    # sliver is dropped and December stands alone.
    one_day <- c("2023-11-30",
                 format(seq(as.Date("2023-12-01"), as.Date("2023-12-31"),
                            by = "day"), "%Y-%m-%d"))
    df1 <- data.frame(ID = "1")
    for (d in one_day) df1[[d]] <- 1

    # The same December, but November now has two days: the sliver survives and
    # is averaged in as though it were a whole month.
    two_day <- c(format(seq(as.Date("2023-11-29"), as.Date("2023-11-30"),
                            by = "day"), "%Y-%m-%d"),
                 format(seq(as.Date("2023-12-01"), as.Date("2023-12-31"),
                            by = "day"), "%Y-%m-%d"))
    df2 <- data.frame(ID = "1")
    for (d in two_day) df2[[d]] <- 1

    expect_equal(calc_par(df1, pars = list(total = sum), agg_period = "month")$total, 31)
    expect_equal(calc_par(df2, pars = list(total = sum), agg_period = "month")$total, 16.5)
})

test_that("month aggregation summarises daily data within each month", {
    dates <- format(seq(as.Date("2023-10-01"), as.Date("2023-12-31"),
                        by = "day"), "%Y-%m-%d")
    df <- data.frame(ID = "1")
    for (d in dates) df[[d]] <- 1

    out <- calc_par(df, pars = list(total = sum), agg_period = "month")
    # three ~monthly bins of roughly 30 daily values each
    expect_equal(nrow(out), 1L)
    expect_gt(out$total, 25)
    expect_lt(out$total, 32)
})

test_that("calc_par propagates NA handling from the supplied function", {
    df <- monthly_df()
    df[[2]] <- c(NA, 10)

    naive <- calc_par(df, pars = list(avg = mean))
    robust <- calc_par(df, pars = list(avg = \(x) mean(x, na.rm = TRUE)))

    expect_true(is.na(naive$avg[1]))
    expect_false(is.na(robust$avg[1]))
})
