# Contract shared by the wide-format weather functions:
#   * `date` is always a Date, never the factor data.table::melt() produces
#   * `ID` / `ID_adm_div` are always character, because they identify a unit
#     rather than measure one

wide <- data.frame(ID = c("1", "2"),
                   X2022.01 = c(1, 30), X2022.02 = c(2, 25),
                   X2022.06 = c(40, 3), X2022.07 = c(38, 4),
                   X2022.12 = c(5, 20), X2023.01 = c(6, 22))

test_that("find_extr_abs_day returns Date, not the factor melt produces", {
    out <- find_extr_abs_day(wide, u_thresh = 10, l_thresh = 5)
    expect_s3_class(out$date, "Date")
    expect_false(is.factor(out$date))
})

test_that("find_extr_rel_day returns Date", {
    out <- find_extr_rel_day(wide, u_thresh = calc_pct_day(wide, p = 0.9))
    expect_s3_class(out$date, "Date")
    expect_false(any(is.na(out$date)))
})

test_that("find_wmo_heatwave returns Date", {
    out <- find_wmo_heatwave(wide, excess = 1, min_spell = 2)
    expect_s3_class(out$date, "Date")
})

test_that("find_spell preserves the Date column", {
    out <- find_spell(find_extr_abs_day(wide, u_thresh = 10), min_spell = 2)
    expect_s3_class(out$date, "Date")
})

test_that("dates parse even when labels are truncated to year and month", {
    # clock::date_parse() returns NA for "2022-01"; the whole date column used
    # to come back NA for monthly series because of it
    out <- find_extr_rel_day(wide, u_thresh = calc_pct_day(wide, p = 0.9))
    expect_false(any(is.na(out$date)))
    expect_equal(min(out$date), as.Date("2022-01-01"))
})

test_that("rows come back in chronological order whatever the column order", {
    shuffled <- wide[, c("ID", "X2023.01", "X2022.06", "X2022.01",
                         "X2022.12", "X2022.02", "X2022.07")]
    out <- find_extr_abs_day(shuffled, u_thresh = 10)
    per_id <- split(out$date, out$ID)

    for (d in per_id) expect_false(is.unsorted(d))
})

test_that("spells are found across columns supplied out of order", {
    # regression: melt's factor levels followed column position, so setorder()
    # left rows unsorted and a real run was never detected
    cols <- c("X2022.04", "X2022.01", "X2022.05", "X2022.02",
              "X2022.06", "X2022.03")
    vals <- c(0, 10, 0, 10, 0, 10)   # highs in Jan, Feb and Mar only
    df <- data.frame(ID = "1")
    for (i in seq_along(cols)) df[[cols[i]]] <- vals[i]

    out <- find_spell(find_extr_abs_day(df, u_thresh = 5), min_spell = 2)
    expect_equal(max(out$spell_abv_5, na.rm = TRUE), 3L)
})

test_that("find_wmo_heatwave sorts before computing spells", {
    cols <- c("2022-01-04", "2022-01-01", "2022-01-05",
              "2022-01-02", "2022-01-06", "2022-01-03")
    vals <- c(10, 30, 10, 30, 10, 30)
    df <- data.frame(ID = "1")
    for (i in seq_along(cols)) df[[cols[i]]] <- vals[i]

    out <- find_wmo_heatwave(df, excess = 1, min_spell = 2)
    expect_false(is.unsorted(out$date))
    expect_equal(max(out$spell_wmo, na.rm = TRUE), 3L)
})

# --- identifier type -------------------------------------------------------

test_that("every ID the package generates is character", {
    coords <- prepare_coord(data.frame(lat = c(1, 2), lon = c(10, 20)), lat, lon)
    expect_type(coords$ID, "character")
})

test_that("ID_adm_div is character wherever it is produced", {
    r <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                     res = 1, crs = "epsg:4326")
    terra::values(r) <- seq_len(terra::ncell(r))
    p <- terra::vect(terra::ext(2, 5, 2, 5), crs = "epsg:4326")

    expect_type(extract_cell_by_poly(r, p)$ID_adm_div, "character")
})

test_that("ID stays character through the extreme-day pipeline", {
    abs_day <- find_extr_abs_day(wide, u_thresh = 10)
    expect_type(abs_day$ID, "character")
    expect_type(calc_pct_day(wide, p = 0.9)$ID, "character")
    expect_type(find_spell(abs_day, min_spell = 2)$ID, "character")
})

test_that("a numeric ID supplied by the caller is not silently reinterpreted", {
    numeric_id <- wide
    numeric_id$ID <- c(1, 2)
    out <- find_extr_abs_day(numeric_id, u_thresh = 10)
    # the package passes the caller's ID through; it does not renumber it
    expect_setequal(unique(out$ID), c(1, 2))
})
