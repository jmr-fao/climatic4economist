# GAUL files sit one level deeper than geoBoundaries and use different field
# names:  root/<ISO>/<folder matching ADM{lvl}>/<file matching ADM[012].geojson>
# with iso3_code, gaul1_name and gaul2_name attributes.
make_gaul_tree <- function(root, iso = "KEN", lvl = 2, n = 3) {
    dir <- file.path(root, iso, paste0(iso, "_ADM", lvl))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    v <- do.call(rbind, lapply(seq_len(n), \(i) {
        terra::vect(terra::ext((i - 1) * 2, (i - 1) * 2 + 2, 0, 2),
                    crs = "epsg:4326")
    }))
    v$iso3_code <- iso
    v$gaul1_name <- paste0("Region_", seq_len(n))
    if (lvl == 2) v$gaul2_name <- paste0("District_", seq_len(n))

    terra::writeVector(v, file.path(dir, paste0(iso, "_ADM", lvl, ".geojson")),
                       filetype = "GeoJSON", overwrite = TRUE)
    root
}

test_that("read_GAUL reads ADM2 and renames the GAUL fields", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2, n = 3)

    out <- read_GAUL(root, iso = "KEN", lvl = 2)

    expect_s4_class(out, "SpatVector")
    expect_equal(nrow(out), 3L)
    expect_true(all(c("ID_adm_div", "iso", "adm_div_1", "adm_div_2") %in% names(out)))
})

test_that("read_GAUL maps gaulN_name onto adm_div_N", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2, n = 3)

    out <- read_GAUL(root, iso = "KEN", lvl = 2)

    expect_equal(out$adm_div_1, paste0("Region_", 1:3))
    expect_equal(out$adm_div_2, paste0("District_", 1:3))
    # the raw GAUL names are gone
    expect_false(any(grepl("^gaul", names(out))))
})

test_that("read_GAUL renames iso3_code to iso", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2)

    out <- read_GAUL(root, iso = "KEN", lvl = 2)

    expect_true("iso" %in% names(out))
    expect_false("iso3_code" %in% names(out))
    expect_equal(unique(out$iso), "KEN")
})

test_that("read_GAUL reads ADM1", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 1, n = 2)

    out <- read_GAUL(root, iso = "KEN", lvl = 1)

    expect_equal(nrow(out), 2L)
    expect_true("adm_div_1" %in% names(out))
    expect_false("adm_div_2" %in% names(out))
})

test_that("read_GAUL assigns a character ID_adm_div", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2, n = 3)

    out <- read_GAUL(root, iso = "KEN", lvl = 2)

    expect_type(out$ID_adm_div, "character")
    expect_equal(out$ID_adm_div, c("1", "2", "3"))
})

test_that("read_GAUL returns a named list for several countries", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2, n = 3)
    make_gaul_tree(root, "UGA", lvl = 2, n = 2)

    out <- read_GAUL(root, iso = c("KEN", "UGA"), lvl = 2)

    expect_type(out, "list")
    expect_named(out, c("KEN", "UGA"))
    expect_equal(nrow(out$KEN), 3L)
    expect_equal(nrow(out$UGA), 2L)
})

test_that("read_GAUL numbers ID_adm_div within each country", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2, n = 3)
    make_gaul_tree(root, "UGA", lvl = 2, n = 2)

    out <- read_GAUL(root, iso = c("KEN", "UGA"), lvl = 2)

    expect_equal(out$KEN$ID_adm_div, c("1", "2", "3"))
    expect_equal(out$UGA$ID_adm_div, c("1", "2"))
})

test_that("read_GAUL unwraps a single country from the list", {
    root <- withr::local_tempdir()
    make_gaul_tree(root, "KEN", lvl = 2)

    expect_s4_class(read_GAUL(root, iso = "KEN", lvl = 2), "SpatVector")
})
