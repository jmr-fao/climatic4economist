# --- calc_pct_spell / find_extr_spell_rel ----------------------------------

# Ten years of daily values with recurring runs of high readings, so spells of
# a few days exist in most months.
spell_source <- function(seed = 11) {
    set.seed(seed)
    dates <- format(seq(as.Date("2010-01-01"), as.Date("2019-12-31"),
                        by = "day"), "%Y-%m-%d")
    df <- data.frame(ID = "1")
    vals <- rnorm(length(dates), mean = 10, sd = 3)
    # inject a run of high values every 40 days
    vals[seq(1, length(vals), by = 40)] <- 25
    vals[seq(2, length(vals), by = 40)] <- 25
    vals[seq(3, length(vals), by = 40)] <- 25
    for (i in seq_along(dates)) df[[dates[i]]] <- vals[i]
    df
}

test_that("calc_pct_spell returns spell percentiles per ID and month", {
    extr <- find_extr_abs_day(spell_source(), u_thresh = 20)
    out <- calc_pct_spell(extr, p = 0.5)

    expect_true(all(c("ID", "month") %in% names(out)))
    expect_equal(dplyr::n_distinct(out$month), 12L)
    expect_true(any(grepl("p$", names(out))))
})

test_that("calc_pct_spell returns integer spell lengths", {
    extr <- find_extr_abs_day(spell_source(), u_thresh = 20)
    out <- calc_pct_spell(extr, p = 0.5)
    pct_cols <- grep("p$", names(out), value = TRUE)

    for (col in pct_cols) expect_type(out[[col]], "integer")
})

test_that("find_extr_spell_rel compares observed spells to the thresholds", {
    extr <- find_extr_abs_day(spell_source(), u_thresh = 20)
    spells <- find_spell(extr, min_spell = 2)
    thr <- calc_pct_spell(extr, p = 0.5)

    out <- find_extr_spell_rel(spells, thr)

    expect_true(all(c("ID", "date") %in% names(out)))
    expect_true(any(grepl("p$", names(out))))
})

test_that("find_extr_spell_rel stops when spells and thresholds disagree", {
    extr <- find_extr_abs_day(spell_source(), u_thresh = 20)
    spells <- find_spell(extr, min_spell = 2)
    # thresholds built on a different extreme value entirely
    other <- find_extr_abs_day(spell_source(), l_thresh = 0.1)
    thr <- calc_pct_spell(other, p = 0.5)

    expect_error(find_extr_spell_rel(spells, thr), "do not match")
})

# --- check_dates_complete --------------------------------------------------

test_that("check_dates_complete accepts a complete daily sequence", {
    dates <- seq.Date(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
    out <- check_dates_complete(dates)

    expect_true(out$all_continuous)
    expect_true(out$year_check)
    expect_equal(nrow(out$month_issues), 0L)
    expect_equal(nrow(out$day_issues), 0L)
})

test_that("check_dates_complete spots a missing day", {
    dates <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")
    dates <- dates[dates != as.Date("2021-06-15")]

    out <- check_dates_complete(dates)

    expect_false(out$all_continuous)
    expect_gt(nrow(out$day_issues), 0L)
})

test_that("check_dates_complete spots a missing month", {
    dates <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")
    dates <- dates[lubridate::month(dates) != 6]

    out <- check_dates_complete(dates)

    expect_false(out$all_continuous)
    expect_gt(nrow(out$month_issues), 0L)
})

test_that("check_dates_complete spots a missing year", {
    dates <- c(seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
               seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"))

    out <- check_dates_complete(dates, freq = "year")
    expect_false(out$year_check)
})

test_that("check_dates_complete can list the missing units", {
    dates <- c(seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
               seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day"))

    out <- check_dates_complete(dates, freq = "year", return_missing = TRUE)
    expect_equal(out$missing_years, 2021)
})

test_that("check_dates_complete accepts a leap year as complete", {
    dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
    out <- check_dates_complete(dates, freq = c("month", "day"))
    expect_equal(nrow(out$month_issues), 0L)
})

# --- compare_adm_div -------------------------------------------------------

test_that("compare_adm_div reports names unique to each side at level 1", {
    x <- data.frame(adm_div_1 = c("A", "B", "C"))
    y <- data.frame(adm_div_1 = c("B", "C", "D"))

    out <- compare_adm_div(x, y, level = 1)

    expect_equal(nrow(out), 1L)
    expect_equal(out$parent, "ALL")
    expect_equal(out$x_only[[1]], "A")
    expect_equal(out$y_only[[1]], "D")
})

test_that("compare_adm_div returns empty sets when the names agree", {
    x <- data.frame(adm_div_1 = c("A", "B"))
    out <- compare_adm_div(x, x, level = 1)

    expect_length(out$x_only[[1]], 0L)
    expect_length(out$y_only[[1]], 0L)
})

test_that("compare_adm_div compares within each parent at level 2", {
    x <- data.frame(adm_div_1 = c("North", "North", "South"),
                    adm_div_2 = c("a", "b", "c"))
    y <- data.frame(adm_div_1 = c("North", "South", "South"),
                    adm_div_2 = c("a", "c", "d"))

    out <- compare_adm_div(x, y, level = 2)

    expect_setequal(out$parent, c("North", "South"))
    expect_equal(out$x_only[out$parent == "North"][[1]], "b")
    expect_equal(out$y_only[out$parent == "South"][[1]], "d")
})

test_that("compare_adm_div does not pool same-named units under different parents", {
    # "Central" under North is a different unit from "Central" under South
    x <- data.frame(adm_div_1 = "North", adm_div_2 = "Central")
    y <- data.frame(adm_div_1 = "South", adm_div_2 = "Central")

    out <- compare_adm_div(x, y, level = 2)

    expect_setequal(out$parent, c("North", "South"))
    expect_equal(out$x_only[out$parent == "North"][[1]], "Central")
    expect_equal(out$y_only[out$parent == "South"][[1]], "Central")
})

test_that("compare_adm_div validates level and required columns", {
    x <- data.frame(adm_div_1 = "A")
    expect_error(compare_adm_div(x, x, level = 1.5))
    expect_error(compare_adm_div(x, x, level = 0))
    expect_error(compare_adm_div(x, x, level = 2))
})

# --- extract_by_poly -------------------------------------------------------

test_that("extract_by_poly aggregates raster values per polygon", {
    r <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                     res = 1, crs = "epsg:4326")
    terra::values(r) <- 1
    names(r) <- "temp"
    p <- rbind(terra::vect(terra::ext(0, 5, 0, 5), crs = "epsg:4326"),
               terra::vect(terra::ext(5, 10, 5, 10), crs = "epsg:4326"))
    p$ID <- c("1", "2")

    out <- extract_by_poly(r, p)

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_equal(out$temp, c(1, 1))
})

test_that("extract_by_poly honours the aggregation function", {
    r <- terra::rast(xmin = 0, xmax = 4, ymin = 0, ymax = 4,
                     res = 1, crs = "epsg:4326")
    terra::values(r) <- seq_len(16)
    names(r) <- "v"
    p <- terra::vect(terra::ext(0, 4, 0, 4), crs = "epsg:4326")
    p$ID <- "1"

    expect_equal(extract_by_poly(r, p, fn_agg = "sum")$v, sum(1:16))
    expect_equal(extract_by_poly(r, p, fn_agg = "mean")$v, mean(1:16))
})

test_that("extract_by_poly rejects an unknown engine", {
    r <- terra::rast(nrows = 4, ncols = 4, crs = "epsg:4326")
    terra::values(r) <- 1
    p <- terra::vect(terra::ext(0, 1, 0, 1), crs = "epsg:4326")
    expect_error(extract_by_poly(r, p, pkg = "nonesuch"))
})

# --- get_poly_attr_for_point -----------------------------------------------

test_that("get_poly_attr_for_point attaches attributes of the containing polygon", {
    poly <- rbind(terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326"),
                  terra::vect(terra::ext(4, 6, 0, 2), crs = "epsg:4326"))
    poly$region <- c("west", "east")
    pts <- data.frame(ID = c("1", "2"), lon = c(1, 5), lat = c(1, 1))

    out <- get_poly_attr_for_point(pts, poly)

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_setequal(out$region, c("west", "east"))
})

test_that("get_poly_attr_for_point falls back to the nearest polygon", {
    poly <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")
    poly$region <- "west"
    pts <- data.frame(ID = c("1", "2"), lon = c(1, 10), lat = c(1, 1))

    out <- expect_output(get_poly_attr_for_point(pts, poly), "outside")

    expect_equal(nrow(out), 2L)
    expect_true(all(out$region == "west"))
})

test_that("get_poly_attr_for_point can drop points outside every polygon", {
    poly <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")
    poly$region <- "west"
    pts <- data.frame(ID = c("1", "2"), lon = c(1, 10), lat = c(1, 1))

    out <- get_poly_attr_for_point(pts, poly, outside = FALSE)
    expect_equal(nrow(out), 1L)
})

test_that("get_poly_attr_for_point prints the iteration label when given one", {
    poly <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")
    poly$region <- "west"
    pts <- data.frame(ID = "1", lon = 1, lat = 1)

    expect_output(get_poly_attr_for_point(pts, poly, iteration = "chunk_2"),
                  "chunk_2")
})

# --- find_folder / find_nested_folder --------------------------------------

test_that("find_folder locates a folder below the named root", {
    root <- withr::local_tempdir()
    anchor <- file.path(root, "anchor")
    dir.create(file.path(anchor, "start", "deep"), recursive = TRUE)
    dir.create(file.path(anchor, "sp_repository"))

    withr::local_dir(file.path(anchor, "start", "deep"))
    found <- find_folder("sp_repository", root = "anchor")

    expect_equal(basename(found), "sp_repository")
})

test_that("find_nested_folder walks a chain of folders", {
    root <- withr::local_tempdir()
    anchor <- file.path(root, "anchor")
    dir.create(file.path(anchor, "start"), recursive = TRUE)
    dir.create(file.path(anchor, "sp_repository", "adm_div"), recursive = TRUE)

    withr::local_dir(file.path(anchor, "start"))
    found <- find_nested_folder("sp_repository", "adm_div", root = "anchor")

    expect_equal(basename(found), "adm_div")
    expect_true(grepl("sp_repository", found, fixed = TRUE))
})

test_that("find_nested_folder with one name matches find_folder", {
    root <- withr::local_tempdir()
    anchor <- file.path(root, "anchor")
    dir.create(file.path(anchor, "start"), recursive = TRUE)
    dir.create(file.path(anchor, "sp_repository"))

    withr::local_dir(file.path(anchor, "start"))
    expect_equal(find_nested_folder("sp_repository", root = "anchor"),
                 find_folder("sp_repository", root = "anchor"))
})
