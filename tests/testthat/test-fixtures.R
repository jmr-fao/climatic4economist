test_that("daily rasters carry dates and units", {
    tmax <- synthetic_daily("tmax")
    pre  <- synthetic_daily("pre")

    expect_equal(terra::nlyr(tmax), 1095)
    expect_equal(range(terra::time(tmax)),
                 as.Date(c("2021-01-01", "2023-12-31")))
    expect_equal(unique(terra::units(tmax)), "K")
    expect_equal(unique(terra::units(pre)), "mm/day")
    expect_true(terra::compareGeom(tmax, pre))
})

test_that("monthly raster spans forty years", {
    pre_month <- synthetic_monthly()

    expect_equal(terra::nlyr(pre_month), 480)
    expect_equal(unique(terra::units(pre_month)), "mm/month")
    expect_equal(min(terra::time(pre_month)), as.Date("1984-01-01"))
})

test_that("households fall inside the grid and the divisions", {
    survey  <- synthetic_survey()
    adm_div <- synthetic_adm_div()
    points  <- terra::vect(survey, geom = c("lon", "lat"), crs = "EPSG:4326")

    expect_equal(nrow(survey), 20)
    expect_s3_class(survey$interview, "Date")
    expect_false(anyNA(terra::extract(synthetic_daily("tmax")[[1]], points)[, 2]))
    expect_true(all(terra::relate(points, adm_div, "within") |> rowSums() == 1))
})
