# --- date_pattern -----------------------------------------------------------

test_that("date_pattern matches every form a date column takes", {
    dates <- c("2022-01-01",    # full date
               "X2023_05_01",   # as read_dta()/make.names() deliver it
               "X2023.12.31",
               "200101",        # year-month, no separator
               "2022-01",       # year-month
               "2022")          # year only

    expect_true(all(grepl(date_pattern(), dates)))
})

test_that("date_pattern excludes survey and metadata columns", {
    others <- c("ID", "ID_adm_div", "hhid", "x_cell", "y_cell",
                "coverage_fraction", "income_2019", "hh_size_2020",
                "pop2015dens", "income_2019_01", "TXmin_2020")

    expect_false(any(grepl(date_pattern(), others)))
})

test_that("date_pattern anchors at the start", {
    # this is what separates a date column from a survey variable: the year
    # leads, rather than appearing somewhere in the name
    expect_true(grepl(date_pattern(), "2019_income"))
    expect_false(grepl(date_pattern(), "income_2019"))
})

# --- to_date ----------------------------------------------------------------

test_that("to_date normalises the separators of a date label", {
    expect_equal(to_date("X2023_05_01"), "2023-05-01")
    expect_equal(to_date("X2023.12.31"), "2023-12-31")
    expect_equal(to_date("2023/12/31"), "2023-12-31")
    expect_equal(to_date("2023-12-31"), "2023-12-31")
})

test_that("to_date strips only a leading X", {
    # an unanchored strip removes the first X anywhere, so these came back as
    # "MA-2020" and "Tmin-2020"
    expect_equal(to_date("MAX_2020"), "MAX-2020")
    expect_equal(to_date("TXmin_2020"), "TXmin-2020")
    expect_equal(to_date("Xmas_2020"), "mas-2020")   # leading X, so stripped
})

test_that("to_date is vectorised and leaves plain names alone", {
    expect_equal(to_date(c("X2023_01_01", "ID", "ID_adm_div")),
                 c("2023-01-01", "ID", "ID-adm-div"))
})

# --- survey columns must not be read as observations ------------------------

test_that("a survey column does not enter the percentile calculation", {
    # income_2019 used to be selected as a date, rewritten to income-2019, and
    # parsed by ymd(truncated = 2) as 2019-01-01: the household's income became
    # a January 2019 weather reading, shifting the median from 13 to 25
    dates_only <- data.frame(
        ID = c("1", "2"),
        `2022-01-01` = c(1, 30), `2022-01-02` = c(25, 2),
        check.names = FALSE
    )
    with_survey <- data.frame(
        ID = c("1", "2"), income_2019 = c(50000, 60000),
        `2022-01-01` = c(1, 30), `2022-01-02` = c(25, 2),
        check.names = FALSE
    )

    expect_equal(as.data.frame(calc_pct_day(with_survey, p = 0.5)),
                 as.data.frame(calc_pct_day(dates_only, p = 0.5)))
})

test_that("survey columns survive the daily and heatwave reshapers", {
    df <- data.frame(
        ID = c("1", "2"), income_2019 = c(50000, 60000),
        `2022-01-01` = c(1, 30), `2022-01-02` = c(25, 2),
        check.names = FALSE
    )

    # whatever the reshapers emit, an income must never appear as a value
    expect_false(any(find_extr_abs_day(df, u_thresh = 20)$value > 1000))
    expect_false(any(find_wmo_heatwave(df, excess = 1)$value > 1000))
})

test_that("merge_with_survey keeps a survey column that merely contains a year", {
    # the old pattern read income_2019_01 as a date column and dropped it
    survey <- data.frame(ID = c("1", "2"), income_2019_01 = c(10, 20))
    values <- data.frame(ID = c("1", "2"), spi = c(0.5, -0.5))

    out <- merge_with_survey(survey, values)

    expect_true("income_2019_01" %in% names(out))
    expect_equal(out$income_2019_01, c(10, 20))
})

test_that("merge_with_survey still drops the wide date columns", {
    survey <- data.frame(ID = c("1", "2"), hhid = 1:2,
                         `2022-01-01` = c(1, 2), check.names = FALSE)
    values <- data.frame(ID = c("1", "2"), spi = c(0.5, -0.5))

    out <- merge_with_survey(survey, values)

    expect_false("2022-01-01" %in% names(out))
    expect_true(all(c("hhid", "spi") %in% names(out)))
})
