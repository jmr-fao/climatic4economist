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

test_that("find_spell lands each spell on the same date whatever the row order", {
    # the spell columns are computed on a re-sorted copy and re-joined by
    # position, so unsorted input used to pair them with the wrong rows
    sorted <- data.frame(
        ID = c("1", "1", "1", "2", "2", "2"),
        date = rep(as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")), 2),
        value = c(1, 2, 3, 4, 5, 6),
        day_abv_90p = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
        day_blw_10p = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
    )
    shuffled <- sorted[c(3, 1, 2, 6, 4, 5), ]

    from_sorted <- find_spell(sorted, min_spell = 2)
    from_shuffled <- find_spell(shuffled, min_spell = 2)

    expect_equal(from_shuffled$ID, from_sorted$ID)
    expect_equal(from_shuffled$date, from_sorted$date)
    expect_equal(from_shuffled$value, from_sorted$value)
    expect_equal(from_shuffled$spell_abv_90p, from_sorted$spell_abv_90p)
    expect_equal(from_shuffled$spell_blw_10p, from_sorted$spell_blw_10p)

    # each unit has one spell of two per indicator, on the day the run ends
    abv <- from_sorted[!is.na(from_sorted$spell_abv_90p), ]
    blw <- from_sorted[!is.na(from_sorted$spell_blw_10p), ]
    expect_equal(abv$date, as.Date(c("2022-01-02", "2022-01-03")))
    expect_equal(blw$date, as.Date(c("2022-01-03", "2022-01-02")))
    expect_true(all(abv$spell_abv_90p == 2L))
    expect_true(all(blw$spell_blw_10p == 2L))
})

test_that("calc_pct_spell is unaffected by the order of the input rows", {
    # the spell falls in January; the shuffle puts February first, which used to
    # be enough to credit the spell to the wrong month
    sorted <- data.frame(
        ID = "1",
        date = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03",
                         "2022-02-01", "2022-02-02", "2022-02-03")),
        value = 1:6,
        day_abv_90p = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )
    shuffled <- sorted[c(4, 5, 6, 1, 2, 3), ]

    out <- calc_pct_spell(sorted, p = 0.5, min_spell = 2)
    jan <- month_label(as.Date("2022-01-01"))
    feb <- month_label(as.Date("2022-02-01"))

    expect_equal(calc_pct_spell(shuffled, p = 0.5, min_spell = 2), out)
    # January holds the three-day spell; February has none and falls back to
    # `min_spell`
    expect_equal(out$spell_abv_90p_50p[out$month == jan], 3L)
    expect_equal(out$spell_abv_90p_50p[out$month == feb], 2L)
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

test_that("WMO spells do not run across unit boundaries", {
    # Unit 1 is hot on the last two days, unit 2 on the first two. The rows are
    # sorted by unit then date, so an ungrouped compute_spell() sees one run of
    # four and credits it all to unit 2.
    df <- data.frame(
        ID = c("1", "2"),
        `2022-01-01` = c(0, 50), `2022-01-02` = c(0, 50),
        `2022-01-03` = c(50, 0), `2022-01-04` = c(50, 0),
        check.names = FALSE
    )

    out <- find_wmo_heatwave(df, excess = 1, min_spell = 2)

    expect_equal(sum(!is.na(out$spell_wmo)), 2L)
    expect_equal(unique(out$spell_wmo[!is.na(out$spell_wmo)]), 2)
    # one spell per unit, each of length two
    expect_equal(out$spell_wmo[out$ID == "1" & out$date == as.Date("2022-01-04")], 2)
    expect_equal(out$spell_wmo[out$ID == "2" & out$date == as.Date("2022-01-02")], 2)
})

test_that("a spell is unaffected by how many other units are present", {
    one <- data.frame(ID = "1",
                      `2022-01-01` = 0, `2022-01-02` = 0,
                      `2022-01-03` = 50, `2022-01-04` = 50,
                      check.names = FALSE)
    two <- rbind(one, data.frame(ID = "2",
                                 `2022-01-01` = 50, `2022-01-02` = 50,
                                 `2022-01-03` = 0, `2022-01-04` = 0,
                                 check.names = FALSE))

    alone <- find_wmo_heatwave(one, excess = 1, min_spell = 2)
    together <- find_wmo_heatwave(two, excess = 1, min_spell = 2)
    together <- together[together$ID == "1", ]

    expect_equal(together$spell_wmo, alone$spell_wmo)
})
