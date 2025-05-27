#' Extract values from a SpatRaster
#'
#' Extract values from a \link[terra]{SpatRaster} for a set of locations.
#' The locations must be a point \link[terra]{SpatVector}.
#'
#' @param raster A data frame. It must contains two variables named `lat` and
#'   `lon`.
#' @param coord character. The coordinate reference system in one of the
#'   following formats: WKT/WKT2, <authority>:<code>, or PROJ-string notation.
#'   See \link[terra]{crs}.
#' @param ... additional arguments to pass to [terra::extract].
#'
#' @returns A  \link[tibble]{tbl_df}, which the extracted values for each
#'   location and the coordinates of the locations. If the raster has multiple
#'   layers, the values of the layer will be split by columns.
#'
#' @seealso \code{\link[terra]{extract}}
#'
#' @export
#'
#' @examples
#' georef_coord(survey, crs = "epsg:4326")

extract_by_coord <- function(raster, coord, ...) {
    terra::extract(raster, coord, xy = TRUE, bind = TRUE, ...) |>
        terra::values() |>
        tibble::as_tibble() |>
        dplyr::rename_with(~gsub("\\.", "_", .x)) |>
        dplyr::relocate(ID, x, y) |>
        dplyr::rename(x_cell = x, y_cell = y)
}
