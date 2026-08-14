test_that("to_date normalises separators and strips the leading X", {
    expect_equal(to_date("X2023_05_01"), "2023-05-01")
    expect_equal(to_date("X2023.12.31"), "2023-12-31")
    expect_equal(to_date("2023/12/31"), "2023-12-31")
    expect_equal(to_date("2023-12-31"), "2023-12-31")
})

test_that("to_date is vectorised", {
    expect_equal(to_date(c("X2020_01_01", "X2021.02.02")),
                 c("2020-01-01", "2021-02-02"))
})

test_that("to_stata_format produces valid Stata names", {
    expect_equal(to_stata_format("2024-01-01"), "X2024_01_01")
    expect_equal(to_stata_format("var.1"), "var1")
    expect_equal(to_stata_format("123_test"), "X123_test")
})

test_that("to_stata_format leaves names that already start with a letter", {
    expect_equal(to_stata_format("temp_mean"), "temp_mean")
})

test_that("is_date recognises parseable dates", {
    expect_true(is_date("2023-01-01"))
    expect_true(is_date("2023/01/01"))
    expect_false(is_date("not a date"))
})

test_that("character_to_clock parses column-name style dates", {
    out <- character_to_clock(c("X2022_01_01", "X2023_02_02"))
    expect_s3_class(out, "Date")
    expect_equal(as.character(out), c("2022-01-01", "2023-02-02"))
})

test_that("second_to_date converts epoch seconds embedded in text", {
    expect_equal(second_to_date("timestamp_1704067200"), "2024-01-01")
    expect_equal(second_to_date(c("time=1704067200", "another_time_1704153600")),
                 c("2024-01-01", "2024-01-02"))
})

test_that("sort_date_columns orders date columns chronologically", {
    df <- data.frame(ID = 1,
                     X2022_03_01 = 3,
                     X2022_01_01 = 1,
                     X2022_02_01 = 2)
    out <- sort_date_columns(df)
    expect_equal(names(out), c("ID", "X2022_01_01", "X2022_02_01", "X2022_03_01"))
    expect_equal(unname(unlist(out[1, -1])), c(1, 2, 3))
})

test_that("sort_date_columns leaves non-date columns in place", {
    df <- data.frame(ID = 1, region = "a", X2022_02_01 = 2, X2022_01_01 = 1)
    expect_equal(names(sort_date_columns(df))[1:2], c("ID", "region"))
})
