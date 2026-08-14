test_that("agg_to_adm_div takes a coverage-weighted mean", {
    df <- data.frame(
        ID_adm_div = c("1", "1", "2", "2"),
        lag = c(0, 0, 0, 0),
        day_abv_90p = c(5, 10, 2, 8),
        coverage_fraction = c(0.6, 0.4, 0.7, 0.3)
    )

    out <- agg_to_adm_div(df, match_col = "^day")

    expect_equal(nrow(out), 2L)
    expect_equal(out$day_abv_90p,
                 c(5 * 0.6 + 10 * 0.4, 2 * 0.7 + 8 * 0.3))
})

test_that("agg_to_adm_div groups by lag as well as by division", {
    df <- data.frame(
        ID_adm_div = c("1", "1", "1", "1"),
        lag = c(0, 0, 1, 1),
        day_abv_90p = c(4, 4, 8, 8),
        coverage_fraction = c(0.5, 0.5, 0.5, 0.5)
    )

    out <- agg_to_adm_div(df, match_col = "^day")

    expect_equal(nrow(out), 2L)
    expect_equal(out$day_abv_90p[out$lag == 0], 4)
    expect_equal(out$day_abv_90p[out$lag == 1], 8)
})

test_that("agg_to_adm_div selects columns by the supplied pattern", {
    df <- data.frame(
        ID_adm_div = c("1", "1"), lag = c(0, 0),
        day_abv_90p = c(2, 4), spell_abv_90p = c(1, 3),
        untouched = c(9, 9),
        coverage_fraction = c(0.5, 0.5)
    )

    out <- agg_to_adm_div(df, match_col = "^day")
    expect_true("day_abv_90p" %in% names(out))
    expect_false("spell_abv_90p" %in% names(out))
})

test_that("agg_to_adm_div carries extra columns through", {
    df <- data.frame(
        ID_adm_div = c("1", "1"), lag = c(0, 0), region = c("north", "north"),
        day_abv_90p = c(2, 4), coverage_fraction = c(0.5, 0.5)
    )

    out <- agg_to_adm_div(df, match_col = "^day", extra_col = region)
    expect_equal(out$region, "north")
})

test_that("agg_to_adm_div ignores missing values when weighting", {
    df <- data.frame(
        ID_adm_div = c("1", "1"), lag = c(0, 0),
        day_abv_90p = c(NA, 8), coverage_fraction = c(0.5, 0.5)
    )
    out <- agg_to_adm_div(df, match_col = "^day")
    expect_equal(out$day_abv_90p, 8)
})

# --- aggregate_frequency ---------------------------------------------------

daily_raster <- function(n_days = 60) {
    r <- terra::rast(nrows = 4, ncols = 4, nlyrs = n_days,
                     xmin = 0, xmax = 4, ymin = 0, ymax = 4)
    terra::values(r) <- rep(seq_len(n_days), each = 16)
    terra::time(r) <- seq(as.Date("2020-01-01"), by = "day", length.out = n_days)
    r
}

test_that("aggregate_frequency collapses daily layers to months", {
    r <- daily_raster(60)   # January and February 2020
    out <- aggregate_frequency(r, target_freq = "%Y-%m", agg_fn = "mean")

    expect_s4_class(out, "SpatRaster")
    expect_equal(terra::nlyr(out), 2L)
    expect_equal(names(out), c("2020-01", "2020-02"))
})

test_that("aggregate_frequency applies the requested function", {
    r <- daily_raster(31)   # January only, values 1..31
    mean_r <- aggregate_frequency(r, target_freq = "%Y-%m", agg_fn = "mean")
    sum_r  <- aggregate_frequency(r, target_freq = "%Y-%m", agg_fn = "sum")

    expect_equal(unname(terra::values(mean_r)[1]), mean(1:31))
    expect_equal(unname(terra::values(sum_r)[1]), sum(1:31))
})

test_that("aggregate_frequency sets the time dimension on the result", {
    out <- aggregate_frequency(daily_raster(60), target_freq = "%Y-%m")
    expect_equal(terra::time(out), as.Date(c("2020-01-01", "2020-02-01")))
})

test_that("aggregate_frequency prints the iteration label when given one", {
    expect_output(aggregate_frequency(daily_raster(31), iteration = "tile_1"),
                  "tile_1")
})

# --- filter_between --------------------------------------------------------

test_that("filter_between keeps rows inside the range, endpoints included", {
    df <- data.frame(date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")))
    dates <- list(start_date = as.Date("2024-01-15"),
                  end_date = as.Date("2024-03-01"))

    out <- filter_between(df, dates)
    expect_equal(out$date, as.Date(c("2024-02-01", "2024-03-01")))
})

test_that("filter_between returns no rows when the range excludes everything", {
    df <- data.frame(date = as.Date(c("2024-01-01", "2024-02-01")))
    dates <- list(start_date = as.Date("2025-01-01"),
                  end_date = as.Date("2025-12-31"))

    expect_equal(nrow(filter_between(df, dates)), 0L)
})

# --- extr_std_index --------------------------------------------------------

test_that("extr_std_index flags values past the supplied thresholds", {
    df <- data.frame(ID = c("1", "2", "3"), value = c(-2, 0, 2))

    out <- extr_std_index(df, l_thrshld = list(-1), u_thrshld = list(1))

    expect_true("std_ind" %in% names(out))
    expect_equal(out$std_ind, c(-2, 0, 2))
    expect_true(any(grepl("abv", names(out))))
    expect_true(any(grepl("blw", names(out))))
})

test_that("extr_std_index skips the bound that is not supplied", {
    df <- data.frame(ID = c("1", "2"), value = c(-2, 2))

    upper_only <- extr_std_index(df, u_thrshld = list(1))
    expect_true(any(grepl("abv", names(upper_only))))
    expect_false(any(grepl("blw", names(upper_only))))

    lower_only <- extr_std_index(df, l_thrshld = list(-1))
    expect_true(any(grepl("blw", names(lower_only))))
    expect_false(any(grepl("abv", names(lower_only))))
})

test_that("extr_std_index strips the minus sign from threshold column names", {
    df <- data.frame(ID = "1", value = -2)
    out <- extr_std_index(df, l_thrshld = list(-1.5))
    # "-1.5" would make an awkward column name, so the sign is removed
    expect_false(any(grepl("-", names(out), fixed = TRUE)))
})
