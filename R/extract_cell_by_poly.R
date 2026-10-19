#' Extract Raster Values by Polygon
#'
#' This function extracts raster values within a given polygon using the
#' `exactextractr` package.
#'
#' @param raster A `SpatRaster` or `RasterLayer` object representing the raster
#'   data to extract.
#' @param poly A `SpatVector` or `sf` object representing the polygon(s) for
#'   extraction.
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
#' tmax <- terra::rast(
#'   system.file("extdata", "tmax_daily.tif", package = "climatic4economist"))
#' adm_div <- terra::vect(
#'   system.file("extdata", "adm_div.gpkg", package = "climatic4economist"))
#'
#' # one row per cell and division, with the share of the cell inside it
#' extract_cell_by_poly(tmax[[1:3]], adm_div)

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
        dplyr::rename(x_cell = x, y_cell = y)
}
