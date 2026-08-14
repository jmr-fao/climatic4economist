lonlat_raster <- function() {
    r <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                     res = 0.1, crs = "epsg:4326")
    terra::values(r) <- seq_len(terra::ncell(r))
    r
}

test_that("crop_with_buffer crops to the vector extent", {
    r <- lonlat_raster()
    v <- terra::vect(terra::ext(4, 5, 4, 5), crs = "epsg:4326")

    out <- crop_with_buffer(r, v, buffer = 0)
    expect_equal(unname(as.vector(terra::ext(out))), c(4, 5, 4, 5))
})

test_that("crop_with_buffer expands the extent in the map units of the data", {
    r <- lonlat_raster()
    v <- terra::vect(terra::ext(4, 5, 4, 5), crs = "epsg:4326")

    out <- crop_with_buffer(r, v, buffer = 0.5)
    expect_equal(unname(as.vector(terra::ext(out))), c(3.5, 5.5, 3.5, 5.5))
})

test_that("crop_with_buffer preserves CRS and resolution", {
    r <- lonlat_raster()
    v <- terra::vect(terra::ext(4, 5, 4, 5), crs = "epsg:4326")

    out <- crop_with_buffer(r, v, buffer = 0.5)
    expect_true(terra::same.crs(out, r))
    expect_equal(terra::res(out), terra::res(r))
})

test_that("crop_with_buffer treats the buffer as meters for a projected raster", {
    r <- terra::rast(xmin = 500000, xmax = 600000,
                     ymin = 4000000, ymax = 4100000,
                     res = 1000, crs = "epsg:32633")
    terra::values(r) <- seq_len(terra::ncell(r))
    v <- terra::vect(terra::ext(540000, 560000, 4040000, 4060000),
                     crs = "epsg:32633")

    out <- crop_with_buffer(r, v, buffer = 5000)
    expect_equal(unname(as.vector(terra::ext(out))),
                 c(535000, 565000, 4035000, 4065000))
})

test_that("crop_with_buffer prints the iteration label when given one", {
    r <- lonlat_raster()
    v <- terra::vect(terra::ext(4, 5, 4, 5), crs = "epsg:4326")
    expect_output(crop_with_buffer(r, v, iteration = "tile_a"), "tile_a")
})

test_that("get_utm_crs picks the zone containing the centroid", {
    v <- terra::vect(terra::ext(10, 11, 45, 46), crs = "epsg:4326")
    crs_string <- terra::crs(get_utm_crs(v), proj = TRUE)
    # longitude 10.5 falls in UTM zone 32, northern hemisphere
    expect_match(crs_string, "\\+zone=32")
    expect_no_match(crs_string, "\\+south")
})

test_that("get_utm_crs marks the southern hemisphere", {
    v <- terra::vect(terra::ext(10, 11, -46, -45), crs = "epsg:4326")
    expect_match(terra::crs(get_utm_crs(v), proj = TRUE), "\\+south")
})

test_that("get_utm_crs rejects non-SpatVector input", {
    expect_error(get_utm_crs(data.frame(x = 1)), "must be a SpatVector")
})

test_that("split_raster returns n_x * n_y named tiles", {
    r <- lonlat_raster()
    tiles <- split_raster(r, n_x = 2, n_y = 2)

    expect_length(tiles, 4L)
    expect_equal(names(tiles), c("tile_001", "tile_002", "tile_003", "tile_004"))
    expect_true(all(vapply(tiles, inherits, logical(1), "SpatRaster")))
})

test_that("split_raster tiles cover the original extent", {
    r <- lonlat_raster()
    tiles <- split_raster(r, n_x = 2, n_y = 2)
    xmins <- vapply(tiles, function(t) terra::xmin(t), numeric(1))
    xmaxs <- vapply(tiles, function(t) terra::xmax(t), numeric(1))

    expect_equal(min(xmins), terra::xmin(r))
    expect_equal(max(xmaxs), terra::xmax(r))
})

test_that("split_raster rejects non-raster input", {
    expect_error(split_raster(data.frame(x = 1)), "must be a terra::SpatRaster")
})

test_that("fill_missing_rast replaces only the missing cells", {
    r <- terra::rast(nrows = 10, ncols = 10)
    terra::values(r) <- 1
    r[5, 5] <- NA

    filled <- fill_missing_rast(r, w = 3, fun = "mean")

    expect_false(is.na(terra::values(filled)[terra::cellFromRowCol(r, 5, 5)]))
    expect_equal(sum(is.na(terra::values(filled))), 0L)
})

test_that("fill_missing_rast rejects an unknown window shape", {
    r <- terra::rast(nrows = 5, ncols = 5)
    terra::values(r) <- 1
    expect_error(fill_missing_rast(r, shape = "hexagon"), "Invalid shape")
})

test_that("georef_coord builds a point SpatVector with one row per ID", {
    df <- data.frame(ID = c("1", "1", "2"),
                     lon = c(10, 10, 11),
                     lat = c(45, 45, 46))
    v <- georef_coord(df, geom = c("lon", "lat"), crs = "epsg:4326")

    expect_s4_class(v, "SpatVector")
    expect_equal(terra::geomtype(v), "points")
    expect_equal(nrow(v), 2L)  # duplicate coordinates collapse
})

test_that("extract_by_coord returns cell values with their coordinates", {
    r <- lonlat_raster()
    names(r) <- "temp"
    pts <- terra::vect(data.frame(ID = c("1", "2"),
                                  lon = c(2.05, 7.05),
                                  lat = c(2.05, 7.05)),
                       geom = c("lon", "lat"), crs = "epsg:4326")

    out <- extract_by_coord(r, pts)

    expect_s3_class(out, "tbl_df")
    expect_true(all(c("x_cell", "y_cell", "temp") %in% names(out)))
    expect_equal(nrow(out), 2L)
})
