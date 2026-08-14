make_wide <- function() {
    data.frame(ID = 1:2,
               X2022.01 = c(1, 10),
               X2022.06 = c(2, 20),
               X2022.11 = c(3, 30),
               X2023.03 = c(4, 40))
}

test_that("select_by_dates keeps identifier columns and the requested range", {
    out <- select_by_dates(make_wide(), from = "2022-06", to = "2022-11")
    expect_equal(names(out), c("ID", "X2022.06", "X2022.11"))
})

test_that("select_by_dates accepts an open-ended range", {
    expect_equal(names(select_by_dates(make_wide(), from = "2022-11")),
                 c("ID", "X2022.11", "X2023.03"))
    expect_equal(names(select_by_dates(make_wide(), to = "2022-06")),
                 c("ID", "X2022.01", "X2022.06"))
})

test_that("select_by_dates returns every date column when no range is given", {
    expect_equal(names(select_by_dates(make_wide())), names(make_wide()))
})

test_that("select_by_season selects months within the year", {
    out <- select_by_season(make_wide(), "Jun", "Nov")
    expect_equal(names(out), c("ID", "X2022.06", "X2022.11"))
})

test_that("select_by_season accepts month numbers and full names", {
    expect_equal(names(select_by_season(make_wide(), 6, 11)),
                 c("ID", "X2022.06", "X2022.11"))
    expect_equal(names(select_by_season(make_wide(), "June", "November")),
                 c("ID", "X2022.06", "X2022.11"))
})

test_that("select_by_season wraps around the turn of the year", {
    # November to March must keep Nov, Jan and Mar but drop Jun
    out <- select_by_season(make_wide(), 11, 3)
    expect_true(all(c("X2022.11", "X2022.01", "X2023.03") %in% names(out)))
    expect_false("X2022.06" %in% names(out))
})

test_that("select_by_interview filters relative to a column of dates", {
    df <- data.frame(
        id = 1:2,
        interview_date = as.Date(c("2023-01-15", "2023-01-15")),
        X2022_12_02 = c(100, 200),
        X2021_06_21 = c(150, 250)
    )
    out <- select_by_interview(df, interview_date, "1 year")
    # only the 2022-12-02 observation falls inside the year before the interview
    expect_true(all(out$date == "X2022_12_02"))
})

test_that("select_by_interview can return wide format", {
    df <- data.frame(
        id = 1:2,
        interview_date = as.Date(c("2023-01-15", "2023-01-15")),
        X2022_12_02 = c(100, 200)
    )
    out <- select_by_interview(df, interview_date, "1 year", wide = TRUE)
    expect_true("X2022_12_02" %in% names(out))
    expect_false("date" %in% names(out))
})
