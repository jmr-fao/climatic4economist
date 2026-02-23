#' Calculate weighted (cost) distance to target features
#'
#' Computes accumulated cost distance from target features across a friction
#' (cost) surface using \code{terra::costDist}. The target features are first
#' rasterized onto the friction raster grid and assigned a sentinel value
#' (-999), which is then used as the origin for cost distance calculation.
#'
#' @param target A \code{SpatVector} or spatial object representing target
#'   locations (e.g., points, lines, or polygons) from which cost distance
#'   will be calculated.
#'
#' @param friction A \code{SpatRaster} representing the friction or cost
#'   surface. Cell values should represent movement cost (positive numeric
#'   values). The raster defines the spatial resolution and extent used
#'   for the analysis.
#'
#' @details
#' All spatial objects must share the same CRS.
#'
#' @return A \code{SpatRaster} containing accumulated cost distance values,
#'   where each cell represents the least-cost distance to the nearest
#'   target feature across the friction surface.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # Example friction surface
#' r <- rast(ncols = 100, nrows = 100)
#' values(r) <- runif(ncell(r), 1, 10)
#'
#' # Example target point
#' pts <- vect(matrix(c(0.5, 0.5), ncol = 2), crs = crs(r))
#'
#' dist_raster <- cal_weighted_distance(pts, r)
#' plot(dist_raster)
#' }
#'
#' @seealso \code{\link[terra]{costDist}}, \code{\link[terra]{rasterize}}
#'
#'
#'#' Calculate weighted (least-cost) distance to target features
#'
#' Computes accumulated least-cost distance from target features across
#' a friction (cost) surface using \code{terra::costDist()}.
#'
#' @param target A \code{SpatVector} representing target locations
#'   (points, lines, or polygons).
#' @param friction A \code{SpatRaster} representing the cost surface.
#'   Values must be positive numeric costs.
#' @param origin_value Numeric value used internally to mark origin cells.
#'   Default is -999. Must not already exist in the friction raster.
#' @param ... additional arguments passed to [terra::costDist]
#'
#' @return A \code{SpatRaster} of accumulated least-cost distances.
#'
#' @seealso \code{\link[terra]{costDist}}
#' @export
#'
calc_weighted_distance <- function(target, friction, origin_value = -999, ...) {

    # Validate inputs
    checkmate::assert_class(friction, "SpatRaster")
    checkmate::assert_class(target, "SpatVector")
    checkmate::assert_choice(directions, c(4, 8))
    checkmate::assert_number(origin_value)

    checkmate::assert_true(terra::same.crs(target, friction),
                           .var.name = "CRS compatibility")

    checkmate::assert_true(terra::relate(terra::ext(target),
                                         terra::ext(friction),
                                         "intersects"),
                           .var.name = "spatial overlap")

    # Compute cost distance
    terra::rasterize(target,
                     friction,
                     field = origin_value) |>
        terra::cover(friction) |>
        terra::costDist(target = origin_value, ...)

}
