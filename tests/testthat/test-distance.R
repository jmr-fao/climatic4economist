polys <- function() {
    p <- rbind(terra::vect(terra::ext(0, 1, 0, 1), crs = "epsg:4326"),
               terra::vect(terra::ext(4, 5, 0, 1), crs = "epsg:4326"))
    p$ID <- c("1", "2")
    p
}

targets <- function() {
    t <- terra::vect(data.frame(lon = c(0.5, 10), lat = c(0.5, 10)),
                     geom = c("lon", "lat"), crs = "epsg:4326")
    t$name <- c("near", "far")
    t
}

test_that("calc_distance returns one distance per feature in x", {
    out <- calc_distance(polys(), targets())

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 2L)
    expect_true("distance" %in% names(out))
})

test_that("calc_distance reports zero where features intersect", {
    out <- calc_distance(polys(), targets())
    # the first target sits inside the first polygon
    expect_equal(out$distance[1], 0)
    expect_gt(out$distance[2], 0)
})

test_that("calc_distance can return geometries instead of a tibble", {
    out <- calc_distance(polys(), targets(), return_geometry = TRUE)
    expect_s4_class(out, "SpatVector")
    expect_true("distance" %in% names(out))
})

test_that("calc_distance returns the full matrix when fun_summ is NULL", {
    out <- calc_distance(polys(), targets(), fun_summ = NULL)
    expect_true(is.matrix(out))
    expect_equal(dim(out), c(2L, 2L))
})

test_that("calc_distance honours the summary function", {
    d_min <- calc_distance(polys(), targets(), fun_summ = min, use_union = FALSE)
    d_max <- calc_distance(polys(), targets(), fun_summ = max, use_union = FALSE)
    expect_true(all(d_max$distance >= d_min$distance))
})

test_that("calc_distance reprojects y when the CRS differ", {
    y <- terra::project(targets(), "epsg:3857")
    expect_message(calc_distance(polys(), y), "CRS do not match")
})

test_that("calc_nearest_distance adds a distance in kilometres", {
    out <- calc_nearest_distance(polys(), targets())

    expect_s3_class(out, "tbl_df")
    expect_true("dist_nearest_km" %in% names(out))
    expect_equal(nrow(out), 2L)
    expect_true(all(out$dist_nearest_km >= 0))
})

test_that("calc_nearest_distance can return geometries", {
    out <- calc_nearest_distance(polys(), targets(), return_geometry = TRUE)
    expect_s4_class(out, "SpatVector")
})

test_that("calc_nearest_distance reprojects y when the CRS differ", {
    y <- terra::project(targets(), "epsg:3857")
    expect_message(calc_nearest_distance(polys(), y), "CRS do not match")
})

# --- calc_road_density -----------------------------------------------------

roads <- function() {
    terra::vect(rbind(cbind(1, 1, c(0.1, 0.9), c(0.5, 0.5))),
                type = "lines", crs = "epsg:4326")
}

test_that("calc_road_density returns length, count, area and density", {
    out <- calc_road_density(polys(), roads())

    expect_s3_class(out, "tbl_df")
    expect_true(all(c("road_length_km", "n_roads", "area_km2", "road_density")
                    %in% names(out)))
    expect_equal(nrow(out), 2L)
})

test_that("calc_road_density gives zero where no road intersects", {
    out <- calc_road_density(polys(), roads())
    # the road lies inside polygon 1 only
    expect_gt(out$road_length_km[out$ID == "1"], 0)
    expect_equal(out$road_length_km[out$ID == "2"], 0)
    expect_equal(out$n_roads[out$ID == "2"], 0)
})

test_that("calc_road_density warns and returns zeros when nothing intersects", {
    far <- terra::vect(rbind(cbind(1, 1, c(50, 51), c(50, 50))),
                       type = "lines", crs = "epsg:4326")
    expect_warning(out <- calc_road_density(polys(), far), "No intersections")
    expect_true(all(out$road_length_km == 0))
})

test_that("calc_road_density can return geometries", {
    out <- calc_road_density(polys(), roads(), return_geometry = TRUE)
    expect_s4_class(out, "SpatVector")
})

test_that("calc_road_density errors when no ID column can be found", {
    p <- polys()
    p$ID <- NULL
    expect_error(calc_road_density(p, roads()), "No valid ID column")
})

# --- calc_weighted_distance ------------------------------------------------

test_that("calc_weighted_distance returns a cost surface", {
    friction <- terra::rast(nrows = 20, ncols = 20,
                            xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                            crs = "epsg:4326")
    terra::values(friction) <- 1
    pts <- terra::vect(matrix(c(0.5, 0.5), ncol = 2), crs = "epsg:4326")

    out <- calc_weighted_distance(pts, friction)

    expect_s4_class(out, "SpatRaster")
    expect_equal(terra::ncell(out), terra::ncell(friction))
    expect_equal(min(terra::values(out), na.rm = TRUE), 0)
})

test_that("calc_weighted_distance validates its inputs", {
    friction <- terra::rast(nrows = 5, ncols = 5, crs = "epsg:4326")
    terra::values(friction) <- 1
    pts <- terra::vect(matrix(c(0, 0), ncol = 2), crs = "epsg:4326")

    expect_error(calc_weighted_distance(pts, "not a raster"))
    expect_error(calc_weighted_distance("not a vector", friction))
})
