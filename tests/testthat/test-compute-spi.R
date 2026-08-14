# SPEI::spi needs a reasonably long series to fit a distribution, so the
# fixture spans 30 years of monthly precipitation for two locations.
spi_df <- function(n_years = 30, n_id = 2, seed = 42) {
    set.seed(seed)
    dates <- format(seq(as.Date("1990-01-01"),
                        by = "month", length.out = n_years * 12), "%Y.%m")
    df <- data.frame(ID = as.character(seq_len(n_id)))
    for (d in dates) df[[d]] <- rgamma(n_id, shape = 2, scale = 20)
    df
}

test_that("compute_spi returns one row per location and keeps the ID", {
    out <- compute_spi(spi_df(), time_scale = 1)

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_true("ID" %in% names(out))
    expect_setequal(out$ID, c("1", "2"))
})

test_that("compute_spi returns one column per input period, in wide format", {
    df <- spi_df()
    date_cols <- grep("[0-9]{4}.[0-9]{2}", names(df), value = TRUE)

    out <- compute_spi(df, time_scale = 1)

    expect_setequal(setdiff(names(out), "ID"), date_cols)
})

test_that("compute_spi produces roughly standardised values", {
    out <- compute_spi(spi_df(n_years = 50), time_scale = 1)
    vals <- unlist(out[, setdiff(names(out), "ID")])
    vals <- vals[is.finite(vals)]

    expect_gt(length(vals), 100)
    expect_lt(abs(mean(vals)), 0.2)
    expect_lt(abs(sd(vals) - 1), 0.3)
})

test_that("compute_spi leaves the first periods undefined at longer time scales", {
    # a 12-month accumulation cannot be formed for the first 11 months
    out <- compute_spi(spi_df(), time_scale = 12)
    first_year <- out[1, 2:12]

    expect_true(all(is.na(unlist(first_year))))
})

test_that("compute_spi carries through the extra identifier columns", {
    df <- spi_df()
    df$x_cell <- c(10, 20)
    df$y_cell <- c(30, 40)

    out <- compute_spi(df, time_scale = 1)

    expect_true(all(c("ID", "x_cell", "y_cell") %in% names(out)))
    expect_equal(out$x_cell, c(10, 20))
})

test_that("compute_spi deduplicates repeated locations", {
    df <- spi_df()
    df <- rbind(df, df)   # each ID appears twice

    out <- compute_spi(df, time_scale = 1)
    expect_equal(nrow(out), 2L)
})

test_that("compute_spi prints the iteration label when given one", {
    expect_output(compute_spi(spi_df(n_years = 30), time_scale = 1,
                              iteration = "site_a"),
                  "site_a")
})

test_that("compute_spi orders output columns chronologically", {
    df <- spi_df()
    date_cols <- grep("[0-9]{4}.[0-9]{2}", names(df), value = TRUE)
    # feed the columns in reversed order
    shuffled <- df[, c("ID", rev(date_cols))]

    out <- compute_spi(shuffled, time_scale = 1)
    out_dates <- setdiff(names(out), "ID")

    expect_equal(out_dates, sort(out_dates))
})
