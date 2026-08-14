# A minimal wide weather table, keyed by whichever column name is asked for.
wide_df <- function(key = "ID") {
    df <- data.frame(
        k = c("1", "2"),
        `2022-01-01` = c(1, 30),
        `2022-01-02` = c(25, 2),
        `2022-02-01` = c(3, 40),
        `2022-02-02` = c(35, 4),
        check.names = FALSE
    )
    names(df)[1] <- key
    df
}

# --- resolve_key ------------------------------------------------------------

test_that("resolve_key prefers ID, then falls back to ID_adm_div", {
    expect_equal(resolve_key(data.frame(ID = 1)), "ID")
    expect_equal(resolve_key(data.frame(ID_adm_div = 1)), "ID_adm_div")
    # both present: the documented precedence decides
    expect_equal(resolve_key(data.frame(ID = 1, ID_adm_div = 2)), "ID")
})

test_that("an explicit id overrides detection and accepts any column name", {
    df <- data.frame(ID = 1, ID_adm_div = 2, village = 3)

    expect_equal(resolve_key(df, "ID_adm_div"), "ID_adm_div")
    expect_equal(resolve_key(df, "village"), "village")
})

test_that("resolve_key rejects an id that is missing or not a single string", {
    df <- data.frame(ID = 1)

    expect_error(resolve_key(df, "nope"), "not found")
    expect_error(resolve_key(df, c("ID", "ID")), "single column name")
    expect_error(resolve_key(df, 42), "single column name")
    expect_error(resolve_key(df, NA_character_), "single column name")
})

test_that("resolve_key names the caller's own argument in its errors", {
    df <- data.frame(village = 1)

    expect_error(resolve_key(df), "Provide `id`")
    expect_error(resolve_key(df, arg = "poly_id"), "Provide `poly_id`")
})

# --- the key travels through the pipeline -----------------------------------

test_that("find_extr_abs_day works off ID_adm_div without being told", {
    out <- find_extr_abs_day(wide_df("ID_adm_div"), u_thresh = 20)

    expect_true("ID_adm_div" %in% names(out))
    expect_false("ID" %in% names(out))
    expect_equal(sum(out$day_abv_20), 4)
})

test_that("a renamed key changes nothing but the key's name", {
    # the strongest statement of intent: the key is a label, not a behaviour
    by_id <- find_extr_abs_day(wide_df("ID"), u_thresh = 20, l_thresh = 5)
    by_adm <- find_extr_abs_day(wide_df("ID_adm_div"), u_thresh = 20,
                                l_thresh = 5)

    names(by_adm)[names(by_adm) == "ID_adm_div"] <- "ID"
    expect_equal(as.data.frame(by_adm), as.data.frame(by_id))
})

test_that("an arbitrary key name works when named explicitly", {
    out <- find_extr_abs_day(wide_df("village"), u_thresh = 20, id = "village")

    expect_true("village" %in% names(out))
    expect_equal(sum(out$day_abv_20), 4)
})

test_that("find_extr_abs_day still errors when no key can be found", {
    expect_error(find_extr_abs_day(wide_df("village"), u_thresh = 20),
                 "No valid ID column")
})

test_that("calc_pct_day works off ID_adm_div, monthly and yearly", {
    df <- wide_df("ID_adm_div")

    mnth <- calc_pct_day(df, p = 0.5)
    expect_true("ID_adm_div" %in% names(mnth))
    expect_equal(nrow(mnth), 4L)   # 2 units x 2 months

    yr <- calc_pct_day(df, p = 0.5, yearly = TRUE)
    expect_true("ID_adm_div" %in% names(yr))
    expect_true(any(grepl("^yr_", names(yr))))
})

test_that("find_wmo_heatwave works off ID_adm_div", {
    out <- find_wmo_heatwave(wide_df("ID_adm_div"), excess = 1, min_spell = 1)

    expect_true("ID_adm_div" %in% names(out))
    expect_true("spell_wmo" %in% names(out))
})

test_that("find_extr_rel_day threads the key through its joins", {
    df <- wide_df("ID_adm_div")
    thresh <- calc_pct_day(df, p = 0.5)

    out <- find_extr_rel_day(df, u_thresh = thresh, unit = "mm")

    expect_true("ID_adm_div" %in% names(out))
    expect_false("ID" %in% names(out))
})

test_that("the whole chain runs on ID_adm_div without naming the key", {
    df <- wide_df("ID_adm_div")

    thresh <- calc_pct_day(df, p = 0.9)
    rel <- find_extr_rel_day(df, u_thresh = thresh, unit = "mm")

    expect_true("ID_adm_div" %in% names(rel))
    expect_equal(sort(unique(rel$ID_adm_div)), c("1", "2"))
})

# --- the joiners ------------------------------------------------------------

test_that("merge_with_survey matches on whichever key it finds", {
    survey <- data.frame(ID_adm_div = c("1", "2"), hhid = 1:2)
    values <- data.frame(ID_adm_div = c("1", "2"), spi = c(0.5, -0.5))

    out <- merge_with_survey(survey, values)

    expect_true("ID_adm_div" %in% names(out))
    expect_equal(nrow(out), 2L)
})

test_that("merge_with_survey says which table is missing the key", {
    survey <- data.frame(ID_adm_div = c("1", "2"), hhid = 1:2)
    values <- data.frame(other = c("1", "2"), spi = c(0.5, -0.5))

    expect_error(merge_with_survey(survey, values),
                 "not found in `new_value`")
})

test_that("find_extr_spell_rel threads the key through its joins", {
    spell <- data.frame(ID_adm_div = c("1", "1"),
                        date = as.Date(c("2022-01-01", "2022-01-02")),
                        spell_blw_0.1 = c(3L, 5L))
    threshold <- data.frame(ID_adm_div = "1", month = "01",
                            spell_blw_0.1_90p = 2L)

    out <- find_extr_spell_rel(spell, threshold)

    expect_true("ID_adm_div" %in% names(out))
    expect_false("ID" %in% names(out))
})

# --- agg_to_adm_div keeps its fixed key -------------------------------------

test_that("agg_to_adm_div says plainly when its key is absent", {
    df <- data.frame(region = c("a", "a"), lag = 0,
                     day_abv_90p = c(5, 10), coverage_fraction = c(.6, .4))

    # the key here is the function's meaning, not a detected label, so it must
    # complain about ID_adm_div rather than fall back to something else
    expect_error(agg_to_adm_div(df, match_col = "^day"), "ID_adm_div")
})
