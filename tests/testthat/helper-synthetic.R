# Synthetic fixtures shared by the v2 tests.
# The files are built by dev/make_fixtures.R and stored in inst/extdata/.

fixture_path <- function(file) {
    path <- system.file("extdata", file, package = "climatic4economist")
    if (!nzchar(path)) {
        stop("Fixture `", file, "` not found; run dev/make_fixtures.R.",
             call. = FALSE)
    }
    path
}

# Daily rasters, 6 x 6 cells, 2021-2023: "tmax" (K) or "pre" (mm/day)
synthetic_daily <- function(variable = c("tmax", "pre")) {
    variable <- match.arg(variable)
    terra::rast(fixture_path(paste0(variable, "_daily.tif")))
}

# Monthly precipitation, 6 x 6 cells, 1984-2023 (mm/month)
synthetic_monthly <- function() {
    terra::rast(fixture_path("pre_monthly.tif"))
}

# Twenty households at twelve locations, interviewed in 2023
synthetic_survey <- function() {
    survey <- utils::read.csv(fixture_path("survey.csv"))
    survey$interview <- as.Date(survey$interview)
    tibble::as_tibble(survey)
}

# Three administrative divisions covering the grid, keyed by ID_adm_div
synthetic_adm_div <- function() {
    terra::vect(fixture_path("adm_div.gpkg"))
}
