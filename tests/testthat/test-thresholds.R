test_that("is_above and is_below flag values past a threshold", {
    x <- data.frame(value = c(5, 10, 15))

    abv <- is_above(x, 10)
    expect_named(abv, "day_abv_10")
    expect_equal(abv$day_abv_10, c(FALSE, FALSE, TRUE))

    blw <- is_below(x, 10)
    expect_named(blw, "day_blw_10")
    expect_equal(blw$day_blw_10, c(TRUE, FALSE, FALSE))
})

test_that("above_threshold and below_threshold return the excess, floored at zero", {
    x <- data.frame(value = c(5, 10, 15))

    abv <- above_threshold(x, 10)
    expect_named(abv, "unit_abv_10")
    expect_equal(abv$unit_abv_10, c(0, 0, 5))

    blw <- below_threshold(x, 10)
    expect_named(blw, "unit_blw_10")
    expect_equal(blw$unit_blw_10, c(5, 0, 0))
})

test_that("the threshold helpers treat the boundary as not exceeded", {
    x <- data.frame(value = 10)
    expect_false(is_above(x, 10)[[1]])
    expect_false(is_below(x, 10)[[1]])
    expect_equal(above_threshold(x, 10)[[1]], 0)
})

test_that("find_extr_abs_day flags days past absolute thresholds", {
    df <- data.frame(ID = c(1, 2),
                     X2022.06 = c(12, 3),
                     X2022.12 = c(10, 15))

    out <- find_extr_abs_day(df, u_thresh = 10, l_thresh = 5)

    expect_true(all(c("ID", "date", "value") %in% names(out)))
    expect_true(any(grepl("^day_abv_10$", names(out))))
    expect_true(any(grepl("^day_blw_5$", names(out))))
    # one row per ID per date
    expect_equal(nrow(out), 4L)
})

test_that("find_extr_abs_day labels the excess columns with `unit`", {
    df <- data.frame(ID = 1, X2022.06 = 12, X2022.12 = 10)
    out <- find_extr_abs_day(df, u_thresh = 10, unit = "mm")
    expect_true(any(grepl("^mm_abv_10$", names(out))))
    expect_false(any(grepl("^unit_abv", names(out))))
})

test_that("find_extr_abs_day returns just the observations when no threshold is given", {
    df <- data.frame(ID = 1, X2022.06 = 12, X2022.12 = 10)
    out <- find_extr_abs_day(df)
    expect_setequal(names(out), c("ID", "date", "value"))
})
