#' Fill Missing Raster Values Using Neighboring Cells
#'
#' Applies a moving window operation that replaces only the missing values (`NA`)
#' in a raster with an aggregate of their surrounding cells.
#'
#' @param raster A `SpatRaster` object from the **terra** package.
#' @param w Integer or matrix. The size of the moving window (default = 3).
#'        If `shape = "square"`, the window will be of size `w x w`.
#'        If `shape = "circle"`, `w` defines the radius of the circular window.
#' @param fun Function or character. The function to apply to neighboring values.
#'        Common choices include `"mean"` (default), `"median"`, `"min"`, `"max"`,
#'        or a user-supplied function.
#' @param shape Character. The shape of the moving window.
#'        Options are `"square"` (default) or `"circle"`.
#'
#' @return A `SpatRaster` with missing values replaced by the aggregated
#'         neighboring values.
#'
#' @details
#' Only cells with missing values (`NA`) are replaced; all other cells
#' remain unchanged. The replacement value is computed using the
#' specified function applied over the defined window of neighboring cells.
#'
#' @seealso [terra::focal]
#'
#' @export
#' @examples
#'
#' \dontrun{
#' library(terra)
#' r <- rast(nrows = 10, ncols = 10)
#' values(r) <- runif(ncell(r))
#' r[sample(1:ncell(r), 10)] <- NA  # insert some missing values
#' r_filled <- fill_missing_rast(r, w = 3, fun = "mean")
#' plot(c(r, r_filled))
#' }

fill_missing_rast <- function(raster, w = 3, fun = "mean", shape = "square") {
    # Define window shape
    if (shape == "square") {
        window <- w
    } else if (shape == "circle") {
        window <- terra::focalMat(raster, w, type = "circle")
    } else {
        stop("Invalid shape. Use 'square' or 'circle'.")
    }

    terra::focal(raster, w = window, fun = fun, na.policy = "only")
}
