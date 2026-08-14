# Build a miniature geoBoundaries-style directory tree:
#   root/<ISO>/<subfolder>/<ISO>_ADM1.geojson
# with the shapeName / shapeGroup / shapeType fields the reader renames.
make_boundary_tree <- function(root, iso = "KEN", n_adm1 = 2, n_adm2 = 4) {
    dir <- file.path(root, iso, "geoBoundaries")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    square <- function(x0, y0, w = 1) {
        terra::vect(terra::ext(x0, x0 + w, y0, y0 + w), crs = "epsg:4326")
    }

    adm1 <- do.call(rbind, lapply(seq_len(n_adm1), \(i) square((i - 1) * 2, 0, 2)))
    adm1$shapeName  <- paste0("Region_", seq_len(n_adm1))
    adm1$shapeGroup <- iso
    adm1$shapeType  <- "ADM1"
    terra::writeVector(adm1, file.path(dir, paste0(iso, "_ADM1.geojson")),
                       filetype = "GeoJSON", overwrite = TRUE)

    # two ADM2 units nested inside each ADM1 square
    adm2 <- do.call(rbind, lapply(seq_len(n_adm2), \(i) {
        parent <- (i - 1) %/% 2
        offset <- ((i - 1) %% 2)
        square(parent * 2 + offset, 0, 1)
    }))
    adm2$shapeName  <- paste0("District_", seq_len(n_adm2))
    adm2$shapeGroup <- iso
    adm2$shapeType  <- "ADM2"
    terra::writeVector(adm2, file.path(dir, paste0(iso, "_ADM2.geojson")),
                       filetype = "GeoJSON", overwrite = TRUE)

    root
}

test_that("read_geoBoundaries reads ADM1 with a single sequential ID", {
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2)

    out <- read_geoBoundaries(root, iso = "KEN", lvl = 1)

    expect_s4_class(out, "SpatVector")
    expect_equal(nrow(out), 2L)
    expect_true(all(c("ID_adm_div", "iso", "adm_div_1") %in% names(out)))
    expect_equal(out$ID_adm_div, c("1", "2"))
    expect_equal(out$adm_div_1, c("Region_1", "Region_2"))
})

test_that("read_geoBoundaries assigns ID_adm_div exactly once", {
    # regression: ID_adm_div used to be assigned in the lvl branch and then
    # overwritten by a second unconditional assignment
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 3)

    out <- read_geoBoundaries(root, iso = "KEN", lvl = 1)

    expect_equal(sum(names(out) == "ID_adm_div"), 1L)
    expect_equal(out$ID_adm_div, c("1", "2", "3"))
    expect_false(anyDuplicated(out$ID_adm_div) > 0)
})

test_that("read_geoBoundaries joins ADM2 to its parent ADM1", {
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2, n_adm2 = 4)

    out <- read_geoBoundaries(root, iso = "KEN", lvl = 2)

    expect_equal(nrow(out), 4L)
    expect_true(all(c("ID_adm_div", "iso", "adm_div_1", "adm_div_2") %in% names(out)))
    expect_equal(out$ID_adm_div, as.character(1:4))
    # each district inherits the region containing it
    expect_equal(out$adm_div_1,
                 c("Region_1", "Region_1", "Region_2", "Region_2"))
})

test_that("read_geoBoundaries returns a named list for several countries", {
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2)
    make_boundary_tree(root, "UGA", n_adm1 = 2)

    out <- read_geoBoundaries(root, iso = c("KEN", "UGA"), lvl = 1)

    expect_type(out, "list")
    expect_named(out, c("KEN", "UGA"))
    expect_equal(out$KEN$iso[1], "KEN")
    expect_equal(out$UGA$iso[1], "UGA")
})

test_that("ID_adm_div restarts at 1 for each country", {
    # the identifier is numbered within a country, not across the whole batch,
    # so two countries of different sizes must each start from "1"
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2)
    make_boundary_tree(root, "UGA", n_adm1 = 4)

    out <- read_geoBoundaries(root, iso = c("KEN", "UGA"), lvl = 1)

    expect_equal(out$KEN$ID_adm_div, c("1", "2"))
    expect_equal(out$UGA$ID_adm_div, c("1", "2", "3", "4"))
})

test_that("ID_adm_div restarts at 1 for each country at ADM2 as well", {
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2, n_adm2 = 4)
    make_boundary_tree(root, "UGA", n_adm1 = 1, n_adm2 = 2)

    out <- read_geoBoundaries(root, iso = c("KEN", "UGA"), lvl = 2)

    expect_equal(out$KEN$ID_adm_div, as.character(1:4))
    expect_equal(out$UGA$ID_adm_div, as.character(1:2))
})

test_that("read_geoBoundaries unwraps a single country from the list", {
    root <- withr::local_tempdir()
    make_boundary_tree(root, "KEN", n_adm1 = 2)

    expect_s4_class(read_geoBoundaries(root, iso = "KEN", lvl = 1), "SpatVector")
})
