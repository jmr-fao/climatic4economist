test_that("cv computes the coefficient of variation", {
    x <- c(10, 15, 20, 25, 30)
    expect_equal(cv(x), sd(x) / mean(x))
})

test_that("cv handles missing values and a zero mean", {
    expect_equal(cv(c(10, 15, NA, 25, 30)),
                 sd(c(10, 15, 25, 30)) / mean(c(10, 15, 25, 30)))
    expect_true(is.na(cv(c(10, 15, NA, 25, 30), na_rm = FALSE)))
    expect_true(is.na(cv(c(-5, 5, -5, 5))))
    expect_true(is.na(cv(c(NA_real_, NA_real_))))
})

test_that("the *_na_check helpers ignore NAs", {
    expect_equal(mean_na_check(c(1, 2, NA, 4, 5)), 3)
    expect_equal(max_na_check(c(1, 2, NA, 4, 5)), 5)
    expect_equal(sum_na_check(c(1, 2, NA, 4, 5)), 12)
})

test_that("the *_na_check helpers fall back to `replace` when all values are NA", {
    all_na <- c(NA_real_, NA_real_, NA_real_)
    expect_equal(mean_na_check(all_na), 0)
    expect_equal(max_na_check(all_na), 0)
    expect_equal(sum_na_check(all_na), 0)
    expect_equal(mean_na_check(all_na, replace = -1), -1)
    expect_equal(max_na_check(all_na, replace = -1), -1)
    expect_equal(sum_na_check(all_na, replace = -1), -1)
})

test_that("quantile_na_check returns named percentiles", {
    out <- quantile_na_check(1:100, p = c(0.25, 0.5))
    expect_named(out, c("25%", "50%"))
    expect_equal(unname(out), unname(quantile(1:100, c(0.25, 0.5))))
})

test_that("quantile_na_check replaces every percentile when all values are NA", {
    out <- quantile_na_check(c(NA_real_, NA_real_), p = c(0.1, 0.9), replace = 7)
    expect_equal(unname(out), c(7, 7))
    expect_named(out, c("10%", "90%"))
})

test_that("quantile_df returns a one-row data frame with p-suffixed names", {
    out <- quantile_df(1:100, p = c(0.25, 0.75), replace = 0)
    expect_equal(nrow(out), 1L)
    expect_named(out, c("25p", "75p"))
})

test_that("calc_mode returns the most frequent value", {
    expect_equal(calc_mode(c(1, 2, 2, 3)), 2)
    expect_equal(calc_mode(c("a", "b", "a")), "a")
    expect_equal(calc_mode(c(NA, 1, 1, 2)), 1)
})

test_that("calc_mode honours the ties argument", {
    x <- c(1, 1, 2, 2)
    expect_equal(calc_mode(x, ties = "first"), 1)
    expect_equal(calc_mode(x, ties = "all"), c(1, 2))
    expect_true(is.na(calc_mode(x, ties = "NA")))
    expect_error(calc_mode(x, ties = "error"), "Multiple modes")
})

test_that("calc_mode returns a typed NA for an all-missing vector", {
    expect_true(is.na(calc_mode(c(NA_real_, NA_real_))))
    expect_type(calc_mode(c(NA_real_, NA_real_)), "double")
})

test_that("calc_mode does not mask base::mode", {
    # calc_mode() used to be exported as mode(), shadowing base::mode()
    expect_equal(mode(1L), "numeric")
    expect_equal(mode("a"), "character")
})

test_that("substitute_l and substitute_u blank out values past a threshold", {
    expect_equal(substitute_l(c(2, 5, 8, 1, 10), threshold = 5),
                 c(NA, 5, 8, NA, 10))
    expect_equal(substitute_u(c(2, 5, 8, 1, 10), threshold = 5),
                 c(2, 5, NA, 1, NA))
})

test_that("substitute_l and substitute_u are no-ops for a NULL threshold", {
    x <- c(2, 5, 8)
    expect_equal(substitute_l(x, threshold = NULL), x)
    expect_equal(substitute_u(x, threshold = NULL), x)
})
