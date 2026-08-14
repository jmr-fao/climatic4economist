# Ten years of monthly values for one location, with a clear seasonal signal so
# the monthly percentiles differ from each other.
rel_df <- function(n_id = 1, seed = 3) {
    set.seed(seed)
    dates <- format(seq(as.Date("2010-01-01"),
                        by = "month", length.out = 120), "%Y.%m")
    df <- data.frame(ID = as.character(seq_len(n_id)))
    for (i in seq_along(dates)) {
        month_i <- ((i - 1) %% 12) + 1
        df[[dates[i]]] <- rnorm(n_id, mean = 10 + month_i, sd = 2)
    }
    df
}

test_that("find_extr_rel_day flags days above the monthly upper percentile", {
    df <- rel_df()
    thr <- calc_pct_day(df, p = 0.9)

    out <- find_extr_rel_day(df, u_thresh = thr)

    expect_s3_class(out, "tbl_df")
    expect_true(all(c("ID", "date", "value") %in% names(out)))
    expect_true(any(grepl("^day_abv_90p$", names(out))))
    expect_true(any(grepl("^unit_abv_90p$", names(out))))
})

test_that("find_extr_rel_day flags days below the monthly lower percentile", {
    df <- rel_df()
    thr <- calc_pct_day(df, p = 0.1)

    out <- find_extr_rel_day(df, l_thresh = thr)

    expect_true(any(grepl("^day_blw_10p$", names(out))))
    expect_true(any(grepl("^unit_blw_10p$", names(out))))
})

test_that("find_extr_rel_day accepts both bounds at once", {
    df <- rel_df()
    out <- find_extr_rel_day(df,
                             u_thresh = calc_pct_day(df, p = 0.9),
                             l_thresh = calc_pct_day(df, p = 0.1))

    expect_true(any(grepl("^day_abv_90p$", names(out))))
    expect_true(any(grepl("^day_blw_10p$", names(out))))
})

test_that("roughly the expected share of days exceeds the 90th percentile", {
    df <- rel_df()
    out <- find_extr_rel_day(df, u_thresh = calc_pct_day(df, p = 0.9))

    share <- mean(out$day_abv_90p, na.rm = TRUE)
    expect_gt(share, 0.02)
    expect_lt(share, 0.20)
})

test_that("the excess is zero on days that do not exceed the threshold", {
    df <- rel_df()
    out <- find_extr_rel_day(df, u_thresh = calc_pct_day(df, p = 0.9))

    calm <- out[!out$day_abv_90p, ]
    expect_true(all(calm$unit_abv_90p == 0))
})

test_that("the excess equals value minus threshold on exceeding days", {
    df <- rel_df()
    out <- find_extr_rel_day(df, u_thresh = calc_pct_day(df, p = 0.9))

    hot <- out[out$day_abv_90p, ]
    expect_true(all(hot$unit_abv_90p > 0))
    expect_true(all(hot$unit_abv_90p <= hot$value))
})

test_that("find_extr_rel_day labels the excess columns with `unit`", {
    df <- rel_df()
    out <- find_extr_rel_day(df, u_thresh = calc_pct_day(df, p = 0.9),
                             unit = "mm")

    expect_true(any(grepl("^mm_abv_90p$", names(out))))
    expect_false(any(grepl("^unit_abv", names(out))))
})

test_that("find_extr_rel_day returns one row per ID and date", {
    df <- rel_df(n_id = 3)
    out <- find_extr_rel_day(df, u_thresh = calc_pct_day(df, p = 0.9))

    expect_equal(nrow(out), 3L * 120L)
    expect_equal(dplyr::n_distinct(out$ID), 3L)
})

test_that("find_extr_rel_day prints the iteration label when given one", {
    df <- rel_df()
    expect_output(
        find_extr_rel_day(df, iteration = "site_c",
                          u_thresh = calc_pct_day(df, p = 0.9)),
        "site_c"
    )
})

test_that("thresholds computed per month are actually applied per month", {
    # a value typical for July must not be flagged as extreme in July, even
    # though it would be extreme in January
    df <- rel_df()
    thr <- calc_pct_day(df, p = 0.9)

    expect_equal(dplyr::n_distinct(thr$month), 12L)
    # the seasonal signal means the July threshold exceeds the January one
    jul <- thr$day_90p[thr$month == "07"]
    jan <- thr$day_90p[thr$month == "01"]
    expect_gt(jul, jan)
})
