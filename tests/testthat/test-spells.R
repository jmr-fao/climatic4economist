test_that("compute_spell reports the spell length on its final day", {
    # three consecutive TRUEs, then a gap, then two consecutive TRUEs
    out <- compute_spell(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), threshold = 2)
    expect_equal(out, c(NA, NA, 3L, NA, NA, 2L))
})

test_that("compute_spell drops runs shorter than the threshold", {
    out <- compute_spell(c(TRUE, FALSE, TRUE, TRUE), threshold = 2)
    expect_equal(out, c(NA, NA, NA, 2L))

    expect_true(all(is.na(compute_spell(c(TRUE, FALSE, TRUE), threshold = 2))))
})

test_that("compute_spell honours a longer threshold", {
    condition <- c(TRUE, TRUE, TRUE, FALSE)
    expect_equal(compute_spell(condition, threshold = 3), c(NA, NA, 3L, NA))
    expect_true(all(is.na(compute_spell(condition, threshold = 4))))
})

test_that("compute_spell returns all NA when nothing satisfies the condition", {
    expect_true(all(is.na(compute_spell(c(FALSE, FALSE, FALSE)))))
})

test_that("find_spell computes spells per ID without bleeding across groups", {
    df <- data.frame(
        ID = c(1, 1, 1, 2, 2, 2),
        date = as.Date(rep(c("2024-01-01", "2024-01-02", "2024-01-03"), 2)),
        value = c(10, 12, 15, 8, 7, 6),
        day_abv_90p = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )

    out <- find_spell(df, min_spell = 2)

    expect_true("spell_abv_90p" %in% names(out))
    # ID 1 has a 3-day spell ending on its last day; ID 2 has none
    expect_equal(out$spell_abv_90p[out$ID == 1], c(NA, NA, 3L))
    expect_true(all(is.na(out$spell_abv_90p[out$ID == 2])))
})

test_that("find_wmo_heatwave flags days above the monthly mean by `excess`", {
    df <- data.frame(ID = 1,
                     `2000-01-01` = 20, `2000-01-02` = 20,
                     `2000-01-03` = 40, `2000-01-04` = 40,
                     check.names = FALSE)

    out <- find_wmo_heatwave(df, excess = 5, min_spell = 2)

    expect_true(all(c("ID", "date", "value", "spell_wmo") %in% names(out)))
    expect_equal(nrow(out), 4L)
    # the two hot days form one spell of length 2, recorded on the last of them
    expect_equal(max(out$spell_wmo, na.rm = TRUE), 2L)
})

test_that("find_wmo_heatwave finds nothing in a flat series", {
    df <- data.frame(ID = 1,
                     `2000-01-01` = 20, `2000-01-02` = 20,
                     `2000-01-03` = 20, `2000-01-04` = 20,
                     check.names = FALSE)
    out <- find_wmo_heatwave(df, excess = 5, min_spell = 2)
    expect_true(all(is.na(out$spell_wmo)))
})
