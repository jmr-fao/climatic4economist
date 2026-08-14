# SPEI is computed on a water balance (precipitation minus evapotranspiration),
# so unlike SPI the input is signed. Thirty years of monthly values.
wb_df <- function(n_years = 30, n_id = 2, seed = 7) {
    set.seed(seed)
    dates <- format(seq(as.Date("1990-01-01"),
                        by = "month", length.out = n_years * 12), "%Y.%m")
    df <- data.frame(ID = as.character(seq_len(n_id)))
    for (d in dates) df[[d]] <- rnorm(n_id, mean = 0, sd = 30)
    df
}

test_that("compute_spei returns one row per location and keeps the ID", {
    out <- compute_spei(wb_df(), time_scale = 1)

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_setequal(out$ID, c("1", "2"))
})

test_that("compute_spei returns one column per input period", {
    df <- wb_df()
    date_cols <- grep("[0-9]{4}.[0-9]{2}", names(df), value = TRUE)

    out <- compute_spei(df, time_scale = 1)
    expect_setequal(setdiff(names(out), "ID"), date_cols)
})

test_that("compute_spei produces roughly standardised values", {
    out <- compute_spei(wb_df(n_years = 50), time_scale = 1)
    vals <- unlist(out[, setdiff(names(out), "ID")])
    vals <- vals[is.finite(vals)]

    expect_gt(length(vals), 100)
    expect_lt(abs(mean(vals)), 0.2)
    expect_lt(abs(sd(vals) - 1), 0.3)
})

test_that("compute_spei leaves the first periods undefined at longer scales", {
    out <- compute_spei(wb_df(), time_scale = 12)
    expect_true(all(is.na(unlist(out[1, 2:12]))))
})

test_that("compute_spei carries through the extra identifier columns", {
    df <- wb_df()
    df$x_cell <- c(10, 20)
    df$y_cell <- c(30, 40)

    out <- compute_spei(df, time_scale = 1)

    expect_true(all(c("ID", "x_cell", "y_cell") %in% names(out)))
    expect_equal(out$x_cell, c(10, 20))
})

test_that("compute_spei deduplicates repeated locations", {
    df <- wb_df()
    out <- compute_spei(rbind(df, df), time_scale = 1)
    expect_equal(nrow(out), 2L)
})

test_that("compute_spei orders output columns chronologically", {
    df <- wb_df()
    date_cols <- grep("[0-9]{4}.[0-9]{2}", names(df), value = TRUE)
    shuffled <- df[, c("ID", rev(date_cols))]

    out_dates <- setdiff(names(compute_spei(shuffled, time_scale = 1)), "ID")
    expect_equal(out_dates, sort(out_dates))
})

test_that("compute_spei prints the iteration label when given one", {
    expect_output(compute_spei(wb_df(), time_scale = 1, iteration = "site_b"),
                  "site_b")
})

test_that("compute_water_balance output feeds straight into compute_spei", {
    pre <- wb_df(seed = 1)
    pet <- wb_df(seed = 2)
    pet[-1] <- -abs(pet[-1])          # evapotranspiration enters as a loss

    wb <- compute_water_balance(pre, pet)
    out <- compute_spei(wb, time_scale = 1)

    expect_equal(nrow(out), 2L)
    expect_setequal(setdiff(names(out), "ID"), setdiff(names(wb), "ID"))
})
