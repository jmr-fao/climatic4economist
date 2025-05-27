#' Extract Raster Values by Polygon
#'
#' This function extracts raster values within a given polygon using the
#' `exactextractr` package.
#'
#' @param raster A `SpatRaster` or `RasterLayer` object representing the raster
#'   data to extract.
#' @param poly A `sf` or `SpatialPolygonsDataFrame` object representing the
#'   polygon(s) for extraction.
#' @param ... additional arguments to pass to [exactextractr::exact_extract].
#'
#' @return A data frame containing extracted values, along with coordinates
#'   (`x`, `y`) and coverage fraction.
#'
#' @seealso \code{\link[exactextractr]{exact_extract}}
#'
#' @export
#'
#' @examples
#' # Example raster
#' r <- terra::rast(nrows=100, ncols=100, xmin=0, xmax=10, ymin=0, ymax=10)
#' terra::values(r) <- runif(ncell(r), min = 0, max = 100)
#'
#' # Example polygon
#' poly <- sf::st_as_sf(sf::st_buffer(st_sfc(sf::st_point(c(5, 5))), dist = 1))
#'
#' # Extract raster values within the polygon
#' extract_by_poly(r, poly)

extract_cell_by_poly <- function(raster, poly, ...) {

    if (any(class(raster) == "SpatRaster")) {
        raster <- raster::brick(raster)
    }
    if (any(class(poly) == "SpatVector")) {
        poly = sf::st_as_sf(poly)
    }
    exactextractr::exact_extract(raster, poly,
                                 include_xy = TRUE,
                                 force_df = TRUE,
                                 ...) |>
        dplyr::bind_rows(.id = "ID_adm_div") |>
        dplyr::relocate(ID_adm_div, x, y, coverage_fraction) |>
        tibble::as_tibble() |>
        dplyr::rename_with(~gsub("\\.", "_", .x)) |>
        dplyr::relocate(ID_adm_div, x, y) |>
        dplyr::rename(x_cell = x, y_cell = y)
}
