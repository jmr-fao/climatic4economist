# Daily extreme indicators for two households, spanning three years before a
# common interview date. One "hot" day per year, so the yearly counts are known.
index_df <- function() {
    dates <- as.Date(c(
        "2023-06-01", "2023-07-01",   # lag 0: within a year of the interview
        "2022-06-01", "2022-07-01",   # lag 1
        "2021-06-01", "2021-07-01"    # lag 2
    ))
    df <- expand.grid(date = dates, ID = c("1", "2"),
                      stringsAsFactors = FALSE)
    df$day_abv_90p <- c(1, 0, 1, 0, 1, 0,
                        1, 1, 0, 0, 0, 0)
    df$temp_abv_90p <- c(2, 0, 3, 0, 4, 0,
                         1, 1, 0, 0, 0, 0)
    df$interview <- as.Date("2024-01-01")
    df
}

test_that("extr_day_index aggregates day counts within each lag", {
    out <- extr_day_index(index_df(), interview = interview, id = ID,
                          interval = "1 year", n_lags = 2)

    expect_true(all(c("ID", "lag") %in% names(out)))
    expect_setequal(unique(out$lag), c(0, 1, 2))
    expect_equal(nrow(out), 6L)   # 2 households x 3 lags
})

test_that("extr_day_index sums the day indicators per lag", {
    out <- extr_day_index(index_df(), interview = interview, id = ID,
                          interval = "1 year", n_lags = 2)
    id1 <- out[out$ID == "1", ]
    id1 <- id1[order(id1$lag), ]

    # household 1 has exactly one hot day in each of the three years
    expect_equal(id1$day_abv_90p_sum, c(1, 1, 1))
})

test_that("extr_day_index sums the unit columns per lag", {
    out <- extr_day_index(index_df(), interview = interview, id = ID,
                          interval = "1 year", n_lags = 2)
    id1 <- out[out$ID == "1", ]
    id1 <- id1[order(id1$lag), ]

    expect_equal(id1$temp_abv_90p_sum, c(2, 3, 4))
})

test_that("extr_day_index keeps only the requested number of lags", {
    out <- extr_day_index(index_df(), interview = interview, id = ID,
                          interval = "1 year", n_lags = 0)

    expect_equal(unique(out$lag), 0)
    expect_equal(nrow(out), 2L)
})

test_that("extr_day_index accepts a literal interview date", {
    df <- index_df()
    df$interview <- NULL

    out <- extr_day_index(df, interview = "2024-01-01", id = ID,
                          interval = "1 year", n_lags = 1)
    expect_setequal(unique(out$lag), c(0, 1))
})

test_that("extr_day_index carries extra columns through unchanged", {
    df <- index_df()
    df$region <- ifelse(df$ID == "1", "north", "south")

    out <- extr_day_index(df, interview = interview, id = ID,
                          interval = "1 year", n_lags = 1,
                          extra_col = region)

    expect_true("region" %in% names(out))
    expect_equal(unique(out$region[out$ID == "1"]), "north")
})

test_that("extr_day_index requires interview, interval and id", {
    df <- index_df()
    expect_error(extr_day_index(df, id = ID, interval = "1 year"), "interview")
    expect_error(extr_day_index(df, interview = interview, id = ID), "interval")
    expect_error(extr_day_index(df, interview = interview, interval = "1 year"), "identifier")
})

test_that("extr_day_index prints the iteration label when given one", {
    expect_output(
        extr_day_index(index_df(), iteration = "batch_1", interview = interview,
                       id = ID, interval = "1 year", n_lags = 1),
        "batch_1"
    )
})

# --- extr_spell_index ------------------------------------------------------

spell_df <- function() {
    dates <- as.Date(c("2023-06-01", "2023-07-01",
                       "2022-06-01", "2022-07-01"))
    df <- expand.grid(date = dates, ID = c("1", "2"),
                      stringsAsFactors = FALSE)
    df$spell_abv_90p <- c(3, 5, 2, NA,
                          NA, NA, 4, 4)
    df$interview <- as.Date("2024-01-01")
    df
}

test_that("extr_spell_index summarises spells with max, mean, sum and count", {
    out <- extr_spell_index(spell_df(), interview = interview, id = ID,
                            interval = "1 year", n_lags = 1)

    expect_true(all(c("spell_abv_90p_max", "spell_abv_90p_mean",
                      "spell_abv_90p_sum", "spell_abv_90p_n") %in% names(out)))
    expect_equal(nrow(out), 4L)   # 2 households x 2 lags
})

test_that("extr_spell_index computes the statistics correctly", {
    out <- extr_spell_index(spell_df(), interview = interview, id = ID,
                            interval = "1 year", n_lags = 1)
    id1_lag0 <- out[out$ID == "1" & out$lag == 0, ]

    expect_equal(id1_lag0$spell_abv_90p_max, 5)
    expect_equal(id1_lag0$spell_abv_90p_mean, 4)
    expect_equal(id1_lag0$spell_abv_90p_sum, 8)
    expect_equal(id1_lag0$spell_abv_90p_n, 2L)
})

test_that("extr_spell_index counts only the non-missing spells", {
    out <- extr_spell_index(spell_df(), interview = interview, id = ID,
                            interval = "1 year", n_lags = 1)
    id2_lag0 <- out[out$ID == "2" & out$lag == 0, ]

    # household 2 has no spell at all in the most recent year
    expect_equal(id2_lag0$spell_abv_90p_n, 0L)
})

test_that("extr_spell_index carries extra columns through unchanged", {
    df <- spell_df()
    df$region <- ifelse(df$ID == "1", "north", "south")

    out <- extr_spell_index(df, interview = interview, id = ID,
                            interval = "1 year", n_lags = 1,
                            extra_col = region)

    expect_true("region" %in% names(out))
    expect_equal(unique(out$region[out$ID == "2"]), "south")
})
