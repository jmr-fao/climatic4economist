test_that("month_label zero-pads every month", {
    dates <- as.Date(paste0("2024-", 1:12, "-01"))
    expect_equal(month_label(dates),
                 c("01", "02", "03", "04", "05", "06",
                   "07", "08", "09", "10", "11", "12"))
})

test_that("month_label accepts Date and POSIXct input", {
    expect_equal(month_label(as.Date("2024-03-15")), "03")
    expect_equal(month_label(as.POSIXct("2024-03-15 12:00:00", tz = "UTC")), "03")
})

test_that("month_label accepts character and factor input", {
    expect_equal(month_label("2024-03-15"), "03")
    expect_equal(month_label(factor("2024-03-15")), "03")
})

test_that("month_label handles months that are not zero padded", {
    # substr(x, 6, 7) returned "1-" here
    expect_equal(month_label("2024-1-15"), "01")
    expect_equal(month_label("2024-9-1"), "09")
})

test_that("month_label handles dates truncated to year and month", {
    # substr(x, 6, 7) returned "1" here, breaking the join against "01"
    expect_equal(month_label("2024-01"), "01")
    expect_equal(month_label("2024-1"), "01")
    expect_equal(month_label("2024-12"), "12")
})

test_that("month_label is consistent across equivalent representations", {
    variants <- c("2024-01-15", "2024-1-15", "2024-01")
    expect_length(unique(month_label(variants)), 1L)
})

test_that("month_label returns NA for unparseable input", {
    out <- month_label(c("2024-03-15", "not a date"))
    expect_equal(out[1], "03")
    expect_true(is.na(out[2]))
    expect_type(out, "character")
})

test_that("month_label preserves NA rather than labelling it", {
    out <- month_label(c(as.Date("2024-03-15"), NA))
    expect_equal(out[1], "03")
    expect_true(is.na(out[2]))
    expect_false(identical(out[2], "NA"))
})

test_that("month_label is vectorised and length preserving", {
    x <- as.Date(c("2024-01-01", "2024-06-15", "2024-12-31"))
    expect_length(month_label(x), 3L)
    expect_equal(month_label(x), c("01", "06", "12"))
})

test_that("month_label crosses a leap day correctly", {
    expect_equal(month_label(as.Date("2024-02-29")), "02")
    expect_equal(month_label("2024-02-29"), "02")
})

test_that("the month key joins percentiles to observations consistently", {
    # calc_pct_day() produces the threshold side and find_extr_rel_day() the
    # observation side; both must label months identically or the join drops rows
    df <- data.frame(ID = "1")
    dates <- format(seq(as.Date("2020-01-01"), as.Date("2022-12-01"),
                        by = "month"), "%Y-%m-%d")
    set.seed(1)
    for (d in dates) df[[d]] <- runif(1, 0, 50)

    thresholds <- calc_pct_day(df, p = 0.9)
    out <- find_extr_rel_day(df, u_thresh = thresholds)

    expect_setequal(unique(thresholds$month),
                    month_label(dates))
    # every observation found a matching threshold
    expect_false(any(is.na(out$day_abv_90p)))
})
