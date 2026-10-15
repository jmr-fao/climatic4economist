## Two cells side by side at the equator, values 10 and 20
two_cells <- function(vals = c(10, 20)) {
    r <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 1, res = 1,
                     crs = "epsg:4326", vals = vals)
    names(r) <- "value"
    r
}

whole_poly <- function() {
    p <- terra::vect(terra::ext(0, 2, 0, 1), crs = "epsg:4326")
    p$id <- 1L
    p
}

# align_count_rast -----------------------------------------------------------

test_that("align_count_rast sums counts onto a coarser grid", {
    pop <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.1,
                       crs = "epsg:4326", vals = 1)
    coarse <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.5,
                          crs = "epsg:4326", vals = 0)

    out <- align_count_rast(pop, coarse)
    expect_true(terra::compareGeom(out, coarse))
    expect_equal(as.vector(terra::values(out)), rep(25, 4))
})

test_that("align_count_rast sums exactly when the template corner is offset", {
    # template starts 3 count cells in: blocks must start there, not at 0
    pop <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, res = 0.1,
                       crs = "epsg:4326", vals = 1)
    pop[1:20, 1:3] <- 100  # three columns of 100 left of the template
    template <- terra::rast(xmin = 0.3, xmax = 1.3, ymin = 0, ymax = 1,
                            res = 0.5, crs = "epsg:4326", vals = 0)

    out <- align_count_rast(pop, template)
    expect_equal(as.vector(terra::values(out)), rep(25, 4))
})

test_that("align_count_rast sums non-nested grids by overlap", {
    pop <- terra::rast(xmin = 0, xmax = 1.2, ymin = 0, ymax = 1.2, res = 0.3,
                       crs = "epsg:4326", vals = 1)
    template <- terra::rast(xmin = 0, xmax = 1.2, ymin = 0, ymax = 1.2,
                            res = 0.4, crs = "epsg:4326", vals = 0)

    out <- align_count_rast(pop, template)
    expect_equal(sum(terra::values(out)), 16, tolerance = 0.01)
})

test_that("align_count_rast repeats counts onto a finer grid", {
    pop <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.5,
                       crs = "epsg:4326", vals = 1:4)
    fine <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.25,
                        crs = "epsg:4326", vals = 0)

    out <- align_count_rast(pop, fine)
    expect_equal(sort(unique(as.vector(terra::values(out)))), 1:4)
})

test_that("align_count_rast sets NA counts to 0", {
    pop <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.5,
                       crs = "epsg:4326", vals = c(NA, 1, 2, 3))

    out <- align_count_rast(pop, pop)
    expect_false(anyNA(terra::values(out)))
    expect_equal(as.vector(terra::values(out))[1], 0)
})

# extract_weighted_by_poly ---------------------------------------------------

test_that("extract_weighted_by_poly area-weights the mean", {
    out <- extract_weighted_by_poly(two_cells(), whole_poly())

    expect_named(out, c("id", "value"))
    expect_equal(out$value, 15, tolerance = 1e-3)
})

test_that("extract_weighted_by_poly weights by population", {
    pop <- terra::rast(two_cells(), vals = c(1, 3))

    out <- extract_weighted_by_poly(two_cells(), whole_poly(), weights = pop)
    expect_equal(out$value, 17.5)
})

test_that("extract_weighted_by_poly falls back to coverage without population", {
    pop <- terra::rast(two_cells(), vals = c(0, 0))

    out <- extract_weighted_by_poly(two_cells(), whole_poly(), weights = pop)
    expect_equal(out$value, 15)
})

test_that("extract_weighted_by_poly aligns weights on another grid", {
    pop <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 1, res = 0.5,
                       crs = "epsg:4326", vals = c(rep(0, 2), rep(1, 2),
                                                   rep(0, 2), rep(1, 2)))
    # all people in the right cell, the one worth 20

    out <- extract_weighted_by_poly(two_cells(), whole_poly(), weights = pop)
    expect_equal(out$value, 20)
})

test_that("extract_weighted_by_poly mode picks the class with most weight", {
    classes <- terra::rast(xmin = 0, xmax = 3, ymin = 0, ymax = 1, res = 1,
                           crs = "epsg:4326", vals = c(1, 1, 2))
    names(classes) <- "class"
    p <- terra::vect(terra::ext(0, 3, 0, 1), crs = "epsg:4326")

    expect_equal(extract_weighted_by_poly(classes, p, stat = "mode")$class, 1)

    pop <- terra::rast(classes, vals = c(1, 1, 5))
    expect_equal(
        extract_weighted_by_poly(classes, p, stat = "mode", weights = pop)$class,
        2
    )
})

test_that("extract_weighted_by_poly counts partial cells by coverage", {
    # the polygon covers all of the left cell and half of the right one
    p <- terra::vect(terra::ext(0, 1.5, 0, 1), crs = "epsg:4326")

    out <- extract_weighted_by_poly(two_cells(), p)
    expect_equal(out$value, (10 + 0.5 * 20) / 1.5, tolerance = 1e-3)
})

test_that("extract_weighted_by_poly returns one column per layer", {
    r <- c(two_cells(), two_cells(c(1, 2)))
    names(r) <- c("a", "b")

    out <- extract_weighted_by_poly(r, whole_poly())
    expect_named(out, c("id", "a", "b"))
})

test_that("extract_weighted_by_poly gives NA where the raster is all NA", {
    out <- extract_weighted_by_poly(two_cells(c(NA, NA)), whole_poly())
    expect_true(is.na(out$value))
})

test_that("extract_weighted_by_poly accepts sf polygons", {
    out <- extract_weighted_by_poly(two_cells(), sf::st_as_sf(whole_poly()))
    expect_equal(out$value, 15, tolerance = 1e-3)
})

# erase_water_bodies ---------------------------------------------------------

water_mask <- function() {
    w <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, res = 0.5,
                     crs = "epsg:4326", vals = 100)
    w[1:2, 1:2] <- 0  # a 1 x 1 degree water body, top left
    w
}

test_that("erase_water_bodies removes a large water body", {
    p <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")
    p$id <- 1L

    out <- erase_water_bodies(p, water_mask())
    share <- terra::expanse(out, unit = "km") / terra::expanse(p, unit = "km")

    expect_equal(nrow(out), 1)
    expect_equal(out$id, 1L)
    expect_equal(share, 0.75, tolerance = 0.01)
})

test_that("erase_water_bodies keeps water bodies below min_km2", {
    p <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")

    out <- erase_water_bodies(p, water_mask(), min_km2 = 1e6)
    expect_equal(terra::expanse(out), terra::expanse(p))
})

test_that("erase_water_bodies leaves polygons on land untouched", {
    w <- terra::rast(water_mask(), vals = 100)
    p <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")

    out <- erase_water_bodies(p, w)
    expect_equal(terra::expanse(out), terra::expanse(p))
})

test_that("erase_water_bodies drops a polygon entirely under water", {
    p <- rbind(terra::vect(terra::ext(0, 1, 1, 2), crs = "epsg:4326"),
               terra::vect(terra::ext(1, 2, 0, 1), crs = "epsg:4326"))
    p$id <- 1:2

    out <- erase_water_bodies(p, water_mask())
    expect_equal(out$id, 2L)
})

# find_year_file -------------------------------------------------------------

series_dir <- function(files) {
    dir <- tempfile("series")
    dir.create(dir)
    file.create(file.path(dir, files))
    dir
}

test_that("find_year_file returns the requested year when present", {
    dir <- series_dir(paste0("pop_", 2015:2020, "_1km.tif"))

    out <- find_year_file(dir, 2017, year_regex = "(?<=pop_)\\d{4}")
    expect_equal(out$src_year, 2017L)
    expect_equal(basename(out$file), "pop_2017_1km.tif")
})

test_that("find_year_file fills a missing year with the nearest one", {
    dir <- series_dir(paste0("pop_", c(2015, 2016, 2020), "_1km.tif"))

    out <- find_year_file(dir, c(2012, 2018, 2025),
                          year_regex = "(?<=pop_)\\d{4}")
    expect_equal(out$year, c(2012L, 2018L, 2025L))
    expect_equal(out$src_year, c(2015L, 2016L, 2020L))  # 2018: tie, earlier
})

test_that("find_year_file returns NA beyond max_gap", {
    dir <- series_dir(paste0("pop_", 2015:2016, "_1km.tif"))

    out <- find_year_file(dir, c(2014, 2010), year_regex = "(?<=pop_)\\d{4}",
                          max_gap = 2)
    expect_equal(out$src_year, c(2015L, NA))
    expect_true(is.na(out$file[2]))
})

test_that("find_year_file reads the data year, not the release year", {
    dir <- series_dir(c("EDGAR_2025_GHG_CO2_2015_emi.tif",
                        "EDGAR_2025_GHG_CO2_2016_emi.tif"))

    out <- find_year_file(dir, 2016, year_regex = "(?<=CO2_)\\d{4}")
    expect_equal(basename(out$file), "EDGAR_2025_GHG_CO2_2016_emi.tif")
})

test_that("find_year_file prefers .tif and reads .tif.gz through /vsigzip/", {
    dir <- series_dir(c("vnl_2013_x.tif.gz", "vnl_2014_x.tif.gz",
                        "vnl_2014_x.tif"))

    out <- find_year_file(dir, 2013:2014, year_regex = "(?<=vnl_)\\d{4}")
    expect_match(out$file[1], "^/vsigzip/.*vnl_2013_x\\.tif\\.gz$")
    expect_match(out$file[2], "vnl_2014_x\\.tif$")
    expect_no_match(out$file[2], "vsigzip")
})

test_that("find_year_file errors when no file matches", {
    dir <- series_dir("readme.txt")
    expect_error(find_year_file(dir, 2015, year_regex = "\\d{4}"),
                 "No file")
})

# iteration ------------------------------------------------------------------

test_that("the extraction helpers print the iteration label when given one", {
    expect_output(align_count_rast(two_cells(), two_cells(), iteration = "BEN"),
                  "Aligning: BEN")
    expect_output(extract_weighted_by_poly(two_cells(), whole_poly(),
                                           iteration = "BEN"),
                  "Extracting: BEN")
    expect_output(erase_water_bodies(whole_poly(), water_mask(),
                                     iteration = "BEN"),
                  "Erasing water bodies: BEN")

    dir <- series_dir("pop_2015_1km.tif")
    expect_output(find_year_file(dir, 2015, year_regex = "(?<=pop_)\\d{4}",
                                 iteration = "BEN"),
                  "Finding year file: BEN")
})

test_that("the extraction helpers print nothing without an iteration label", {
    expect_silent(extract_weighted_by_poly(two_cells(), whole_poly()))
})
