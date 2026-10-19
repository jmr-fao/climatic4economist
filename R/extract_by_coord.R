#' Extract values from a SpatRaster
#'
#' Extract values from a \link[terra]{SpatRaster} for a set of locations.
#' The locations must be a point \link[terra]{SpatVector}.
#'
#' @param raster A \link[terra]{SpatRaster}. Each layer becomes one column of
#'   the result.
#' @param coord A point \link[terra]{SpatVector} carrying the unit identifier,
#'   typically created with [georef_coord()].
#' @param id Optional character string naming the column that identifies the
#'   units. When `NULL`, `ID` is used, or `ID_adm_div` when that is the only
#'   one present.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
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
#' tmax <- terra::rast(
#'   system.file("extdata", "tmax_daily.tif", package = "climatic4economist"))
#' survey <- read.csv(
#'   system.file("extdata", "survey.csv", package = "climatic4economist"))
#' coord <- prepare_coord(survey, lat_var = lat, lon_var = lon)
#' points <- georef_coord(coord, geom = c("lon", "lat"), crs = "EPSG:4326")
#'
#' extract_by_coord(tmax[[1:5]], points)

extract_by_coord <- function(raster, coord, iteration = NULL, id = NULL, ...) {
    if (!is.null(iteration)) cat("Extracting:", iteration, "\n")

    out <- terra::extract(raster, coord, xy = TRUE, bind = TRUE, ...) |>
        terra::values() |>
        tibble::as_tibble() |>
        dplyr::rename_with(~gsub("\\.", "_", .x))

    key <- resolve_key(out, id)

    out |>
        dplyr::relocate(dplyr::all_of(key), x, y) |>
        dplyr::rename(x_cell = x, y_cell = y)
}
