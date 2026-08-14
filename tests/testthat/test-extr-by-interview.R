# Two locations with daily extreme indicators spanning three years before a
# common interview date. One "hot" day per year for location 1, so the yearly
# counts are known. Mirrors the fixture in test-extr-index.R, but split into the
# two tables the *_by_interview() functions take.
day_indicators <- function() {
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
    df
}

day_survey <- function() {
    data.frame(hhid      = 1:2,
               ID        = c("1", "2"),
               interview = as.Date("2024-01-01"),
               stringsAsFactors = FALSE)
}

test_that("extr_day_by_interview aggregates day counts within each lag", {
    out <- extr_day_by_interview(day_survey(), day_indicators(),
                                 by = ID, id = hhid, interview = interview,
                                 interval = "1 year", n_lags = 2)

    expect_true(all(c("hhid", "lag") %in% names(out)))
    expect_setequal(unique(out$lag), c(0, 1, 2))
    expect_equal(nrow(out), 6L)   # 2 households x 3 lags
})

test_that("extr_day_by_interview sums the day indicators per lag", {
    out <- extr_day_by_interview(day_survey(), day_indicators(),
                                 by = ID, id = hhid, interview = interview,
                                 interval = "1 year", n_lags = 2)
    hh1 <- out[out$hhid == 1, ]
    hh1 <- hh1[order(hh1$lag), ]

    # household 1 has exactly one hot day in each of the three years
    expect_equal(hh1$day_abv_90p_sum, c(1, 1, 1))
    expect_equal(hh1$temp_abv_90p_sum, c(2, 3, 4))
})

test_that("extr_day_by_interview keeps only the requested number of lags", {
    out <- extr_day_by_interview(day_survey(), day_indicators(),
                                 by = ID, id = hhid, interview = interview,
                                 interval = "1 year", n_lags = 0)

    expect_equal(unique(out$lag), 0)
    expect_equal(nrow(out), 2L)
})

test_that("extr_day_by_interview accepts a literal interview date", {
    survey <- day_survey()
    survey$interview <- NULL

    out <- extr_day_by_interview(survey, day_indicators(),
                                 by = ID, id = hhid, interview = "2024-01-01",
                                 interval = "1 year", n_lags = 1)

    expect_setequal(unique(out$lag), c(0, 1))
})

test_that("extr_day_by_interview carries extra columns through unchanged", {
    survey <- day_survey()
    survey$region <- c("north", "south")

    out <- extr_day_by_interview(survey, day_indicators(),
                                 by = ID, id = hhid, interview = interview,
                                 interval = "1 year", n_lags = 1,
                                 extra_col = region)

    expect_true("region" %in% names(out))
    expect_equal(unique(out$region[out$hhid == 1]), "north")
})

test_that("extr_day_by_interview requires by, id, interview and interval", {
    survey <- day_survey()
    ind <- day_indicators()

    expect_error(extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                       interval = "1 year"), "interview")
    expect_error(extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                       interview = interview), "interval")
    expect_error(extr_day_by_interview(survey, ind, by = ID,
                                       interview = interview,
                                       interval = "1 year"), "identifier")
    expect_error(extr_day_by_interview(survey, ind, id = hhid,
                                       interview = interview,
                                       interval = "1 year"), "linking")
})

test_that("extr_day_by_interview requires a date column in the indicators", {
    ind <- day_indicators()
    names(ind)[names(ind) == "date"] <- "day"

    expect_error(
        extr_day_by_interview(day_survey(), ind, by = ID, id = hhid,
                              interview = interview, interval = "1 year"),
        "`date` column"
    )
})

test_that("extr_day_by_interview prints the iteration label when given one", {
    expect_output(
        extr_day_by_interview(day_survey(), day_indicators(),
                              by = ID, id = hhid, interview = interview,
                              interval = "1 year", iteration = "batch_1"),
        "batch_1"
    )
})

# --- equivalence with the merge-first path ---------------------------------

test_that("extr_day_by_interview matches extr_day_index", {
    survey <- day_survey()
    ind <- day_indicators()

    merged <- merge_with_survey(survey, ind)
    old <- extr_day_index(merged, interview = interview, id = hhid,
                          interval = "1 year", n_lags = 2)
    old <- old[order(old$hhid, old$lag), ]

    new <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year",
                                 n_lags = 2)
    new <- new[order(new$hhid, new$lag), ]

    cols <- c("hhid", "lag", "day_abv_90p_sum", "temp_abv_90p_sum")
    expect_equal(as.data.frame(new[, cols]), as.data.frame(old[, cols]),
                 ignore_attr = TRUE)
})

test_that("extr_spell_by_interview matches extr_spell_index", {
    dates <- as.Date(c("2023-06-01", "2023-07-01",
                       "2022-06-01", "2022-07-01"))
    spells <- expand.grid(date = dates, ID = c("1", "2"),
                          stringsAsFactors = FALSE)
    spells$spell_abv_90p <- c(3, 5, 2, NA,
                              NA, NA, 4, 4)
    survey <- day_survey()

    merged <- merge_with_survey(survey, spells)
    old <- extr_spell_index(merged, interview = interview, id = hhid,
                            interval = "1 year", n_lags = 1)
    old <- old[order(old$hhid, old$lag), ]

    new <- extr_spell_by_interview(survey, spells, by = ID, id = hhid,
                                   interview = interview, interval = "1 year",
                                   n_lags = 1)
    new <- new[order(new$hhid, new$lag), ]

    cols <- c("hhid", "lag", "spell_abv_90p_max", "spell_abv_90p_mean",
              "spell_abv_90p_sum", "spell_abv_90p_n")
    expect_equal(as.data.frame(new[, cols]), as.data.frame(old[, cols]),
                 ignore_attr = TRUE)
})

# --- boundary conventions --------------------------------------------------

test_that("the interview date itself is excluded from lag 0", {
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID = "1",
                      date = as.Date(c("2024-01-01", "2023-12-31")),
                      day_abv_90p = c(1, 1))

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year")

    # only 2023-12-31 counts; the interview day is never in its own period
    expect_equal(out$day_abv_90p_sum, 1)
})

test_that("a date on the anniversary belongs to the older lag", {
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID = "1",
                      date = as.Date(c("2023-01-01", "2022-12-31")),
                      day_abv_90p = c(1, 1))

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year",
                                 n_lags = 1)

    # 2023-01-01 is a *complete* year before the interview, so it counts as
    # lag 1 rather than lag 0, and 2022-12-31 joins it. Lag 0 has no data, so
    # it produces no row. Matches extr_day_index().
    expect_equal(nrow(out), 1L)
    expect_equal(out$lag, 1L)
    expect_equal(out$day_abv_90p_sum, 2)
})

test_that("month-end interviews produce contiguous windows", {
    # 2023-02-28 shifted back by whole years stays on the 28th; the lag 2
    # window then reaches into the leap year 2020 and picks up 29 February.
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2023-02-28"))
    ind <- data.frame(ID = "1",
                      date = seq.Date(as.Date("2020-01-01"),
                                      as.Date("2023-02-28"), by = "day"))
    ind$day_abv_90p <- 1

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year",
                                 n_lags = 2)
    out <- out[order(out$lag), ]

    expect_equal(nrow(out), 3L)
    expect_false(any(is.na(out$day_abv_90p_sum)))
    # lag 0 loses the interview day itself; lag 2 gains 29 February 2020
    expect_equal(out$day_abv_90p_sum, c(364, 365, 366))
    # the three windows tile the span without gaps or overlaps
    expect_equal(sum(out$day_abv_90p_sum),
                 as.numeric(as.Date("2023-02-27") - as.Date("2020-02-29")) + 1)
})

test_that("leap-boundary lags differ from extr_day_index by design", {
    # Both approaches tile the reference span without gaps; they disagree only
    # on which lag owns 29 February. extr_day_index() reads it as three whole
    # years before 2023-02-28 (the third anniversary rolls back onto the
    # interview date) and assigns lag 3. Cutting windows at 28 February of each
    # year instead places it one day inside lag 2. This test pins the choice.
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2023-02-28"))
    ind <- data.frame(ID = "1", date = as.Date("2020-02-29"),
                      day_abv_90p = 1)

    new <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year",
                                 n_lags = 2)
    old <- extr_day_index(merge_with_survey(survey, ind),
                          interview = interview, id = hhid,
                          interval = "1 year", n_lags = 2)

    expect_equal(new$lag, 2L)          # window approach: inside lag 2
    expect_equal(nrow(old), 0L)        # find_lag approach: dropped as lag 3
})

test_that("a leap day interview is handled", {
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2024-02-29"))
    ind <- data.frame(ID = "1",
                      date = as.Date(c("2024-02-28", "2023-03-01")),
                      day_abv_90p = c(1, 1))

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year")

    expect_equal(out$day_abv_90p_sum, 2)
})

test_that("units sharing a location and interview date get identical results", {
    survey <- data.frame(hhid = 1:2, ID = c("1", "1"),
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID = "1", date = as.Date("2023-06-01"),
                      day_abv_90p = 1)

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year")

    expect_equal(nrow(out), 2L)
    expect_equal(length(unique(out$day_abv_90p_sum)), 1L)
})

test_that("units with a missing or out-of-range interview are dropped", {
    survey <- data.frame(hhid = 1:3, ID = c("1", "1", "1"),
                         interview = as.Date(c("2024-01-01", NA, "1990-01-01")))
    ind <- data.frame(ID = "1", date = as.Date("2023-06-01"),
                      day_abv_90p = 1)

    out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                 interview = interview, interval = "1 year")

    expect_equal(out$hhid, 1L)
})

test_that("the by column need not be called ID", {
    survey <- data.frame(hhid = 1L, ID_adm_div = "a",
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID_adm_div = "a", date = as.Date("2023-06-01"),
                      day_abv_90p = 1)

    out <- extr_day_by_interview(survey, ind, by = ID_adm_div, id = hhid,
                                 interview = interview, interval = "1 year")

    expect_equal(out$day_abv_90p_sum, 1)
})

test_that("old and new agree on every interview date but 28 February", {
    # Sweeps three years of interview dates against a continuous daily series.
    # The two functions may only part company where an anniversary rolls back
    # onto a leap day; anywhere else a disagreement means one of them broke.
    days <- seq.Date(as.Date("2016-01-01"), as.Date("2024-12-31"), by = "day")
    ind <- data.frame(ID = "1", date = days, day_abv_90p = 1)
    interviews <- seq.Date(as.Date("2022-01-01"), as.Date("2024-12-31"),
                           by = "day")

    disagree <- vapply(interviews, function(iv) {
        survey <- data.frame(hhid = 1L, ID = "1", interview = iv)

        new <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                     interview = interview,
                                     interval = "1 year", n_lags = 3)
        old <- extr_day_index(merge_with_survey(survey, ind),
                              interview = interview, id = hhid,
                              interval = "1 year", n_lags = 3)

        new <- new$day_abv_90p_sum[order(new$lag)]
        old <- old$day_abv_90p_sum[order(old$lag)]

        # a differing split must still cover the same number of days
        if (!identical(sum(new), sum(old))) return(NA_character_)
        if (isTRUE(all.equal(new, old))) NA_character_ else format(iv)
    }, character(1))

    expect_setequal(disagree[!is.na(disagree)],
                    c("2022-02-28", "2023-02-28"))
})

# --- interval parsing ------------------------------------------------------

test_that("intervals are accepted spaced, unspaced and pluralised", {
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID = "1", date = as.Date("2023-12-15"),
                      day_abv_90p = 1)

    for (int in c("1 month", "1month", "2 months")) {
        out <- extr_day_by_interview(survey, ind, by = ID, id = hhid,
                                     interview = interview, interval = int)
        expect_equal(out$day_abv_90p_sum, 1, info = int)
    }
})

test_that("an interval without a number is rejected", {
    survey <- data.frame(hhid = 1L, ID = "1",
                         interview = as.Date("2024-01-01"))
    ind <- data.frame(ID = "1", date = as.Date("2023-12-15"),
                      day_abv_90p = 1)

    expect_error(
        extr_day_by_interview(survey, ind, by = ID, id = hhid,
                              interview = interview, interval = "year"),
        "must contain a number"
    )
})
