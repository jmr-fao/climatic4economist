# Build the synthetic fixtures used by tests, examples and vignettes
#
# 1. A 6 x 6 grid over a small area, EPSG:4326, 0.2 degree cells.
# 2. Daily maximum temperature (K) and precipitation (mm/day), 2021-2023.
# 3. Monthly precipitation (mm/month), 1984-2023, for SPI/SPEI.
# 4. Twenty households at twelve locations, interviewed in 2023.
# 5. Three administrative divisions covering the grid.
#
# Run from the package root: Rscript dev/make_fixtures.R
# Output goes to inst/extdata/. Everything is seeded, so reruns are identical.

# ==== Set up ============================== ----
## Paths                                   ----
path_to_extdata <- file.path("inst", "extdata")
dir.create(path_to_extdata, showWarnings = FALSE, recursive = TRUE)

## Global options                          ----
set.seed(20261016)
grid_template <- terra::rast(
    nrows = 6,
    ncols = 6,
    xmin  = 30,
    xmax  = 31.2,
    ymin  = -1,
    ymax  = 0.2,
    crs   = "EPSG:4326"
)
n_cell <- terra::ncell(grid_template)

# ==== Daily rasters ======================= ----
## Dates                                   ----
daily_dates <- seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day")
n_day       <- length(daily_dates)
day_of_year <- as.integer(format(daily_dates, "%j"))

## Maximum temperature                     ----
# seasonal cycle plus a west-east gradient plus noise, in Kelvin
cell_offset <- rep(seq(-1.5, 1.5, length.out = 6), times = 6)
season      <- 6 * sin(2 * pi * (day_of_year - 100) / 365)
tmax_values <- outer(cell_offset, 300 + season, `+`) +
    matrix(stats::rnorm(n_cell * n_day, sd = 2), nrow = n_cell)

tmax <- terra::rast(grid_template, nlyrs = n_day)
terra::values(tmax) <- tmax_values
terra::time(tmax)     <- daily_dates
terra::units(tmax)    <- "K"
terra::varnames(tmax) <- "tmax"
names(tmax)           <- format(daily_dates)

## Precipitation                           ----
# about 40 percent wet days, gamma amounts, wetter in the second half of the year
wet_prob   <- 0.3 + 0.2 * (day_of_year > 182)
is_wet     <- matrix(
    stats::runif(n_cell * n_day) < rep(wet_prob, each = n_cell),
    nrow = n_cell
)
pre_values <- is_wet * matrix(
    stats::rgamma(n_cell * n_day, shape = 0.8, scale = 10),
    nrow = n_cell
)

pre <- terra::rast(grid_template, nlyrs = n_day)
terra::values(pre) <- round(pre_values, 1)
terra::time(pre)     <- daily_dates
terra::units(pre)    <- "mm/day"
terra::varnames(pre) <- "pre"
names(pre)           <- format(daily_dates)

# ==== Monthly raster ====================== ----
monthly_dates <- seq(as.Date("1984-01-01"), as.Date("2023-12-01"), by = "month")
n_month       <- length(monthly_dates)
month_number  <- as.integer(format(monthly_dates, "%m"))
month_mean    <- 60 + 50 * sin(2 * pi * (month_number - 3) / 12)

pre_month_values <- matrix(
    stats::rgamma(n_cell * n_month, shape = 2, scale = rep(pmax(month_mean, 5) / 2, each = n_cell)),
    nrow = n_cell
)

pre_month <- terra::rast(grid_template, nlyrs = n_month)
terra::values(pre_month) <- round(pre_month_values, 1)
terra::time(pre_month)     <- monthly_dates
terra::units(pre_month)    <- "mm/month"
terra::varnames(pre_month) <- "pre"
names(pre_month)           <- format(monthly_dates, "%Y-%m")

# ==== Survey ============================== ----
# twelve distinct locations inside the grid, some shared by several households
location <- data.frame(
    lon = round(stats::runif(12, 30.05, 31.15), 4),
    lat = round(stats::runif(12, -0.95, 0.15), 4)
)
survey <- data.frame(
    hhid      = sprintf("HH%02d", 1:20),
    location  = c(1:12, sample(1:12, 8, replace = TRUE))
)
survey$lon       <- location$lon[survey$location]
survey$lat       <- location$lat[survey$location]
survey$interview <- as.Date("2023-03-01") +
    sample(0:200, 20, replace = TRUE)
survey$location  <- NULL

# ==== Administrative divisions ============ ----
adm_div <- terra::vect(
    c(
        "POLYGON ((30 -1, 30.6 -1, 30.6 0.2, 30 0.2, 30 -1))",
        "POLYGON ((30.6 -1, 31.2 -1, 31.2 -0.4, 30.6 -0.4, 30.6 -1))",
        "POLYGON ((30.6 -0.4, 31.2 -0.4, 31.2 0.2, 30.6 0.2, 30.6 -0.4))"
    ),
    crs = "EPSG:4326"
)
adm_div$ID_adm_div <- c("1", "2", "3")
adm_div$adm_div_1  <- c("West", "East", "East")
adm_div$adm_div_2  <- c("West A", "East South", "East North")

# ==== Write =============================== ----
wopt <- list(gdal = c("COMPRESS=ZSTD", "PREDICTOR=3"), datatype = "FLT4S")

terra::writeRaster(tmax, file.path(path_to_extdata, "tmax_daily.tif"),
                   overwrite = TRUE, wopt = wopt)
terra::writeRaster(pre, file.path(path_to_extdata, "pre_daily.tif"),
                   overwrite = TRUE, wopt = wopt)
terra::writeRaster(pre_month, file.path(path_to_extdata, "pre_monthly.tif"),
                   overwrite = TRUE, wopt = wopt)
utils::write.csv(survey, file.path(path_to_extdata, "survey.csv"),
                 row.names = FALSE)
terra::writeVector(adm_div, file.path(path_to_extdata, "adm_div.gpkg"),
                   overwrite = TRUE)
