#' Split a Raster into Smaller Tiles
#'
#' This function divides a `SpatRaster` object into a grid of smaller tiles,
#' optionally adding overlap between adjacent tiles. It is useful for parallel
#' processing, memory management, or spatial subsetting of large rasters.
#'
#' @param rast A `terra::SpatRaster` object to be divided.
#' @param n_x Integer. Number of splits (tiles) along the x-axis (longitude).
#' @param n_y Integer. Number of splits (tiles) along the y-axis (latitude).
#' @param overlap Numeric. Optional overlap between adjacent tiles, in map units
#'   of the raster’s coordinate reference system (e.g., meters or degrees).
#'
#' @details
#' The raster is divided evenly along the x and y axes according to `n_x` and
#' `n_y`. Each resulting tile is cropped from the original raster based on the
#' computed sub-extent. If `overlap > 0`, each tile is extended by the specified
#' distance in all directions before cropping, which can be useful to avoid edge
#' effects in analyses requiring neighborhood operations.
#'
#' @return A named list of `SpatRaster` objects, each representing a tile of the
#'   original raster. Tile names follow the pattern `"tile_001"`, `"tile_002"`, etc.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r <- rast(ncols = 100, nrows = 100)
#' values(r) <- runif(ncell(r))
#'
#' # Split into a 4x4 grid without overlap
#' tiles <- split_raster(r, n_x = 4, n_y = 4)
#'
#' # Split into a 2x2 grid with 10-meter overlap
#' tiles_overlap <- split_raster(r, n_x = 2, n_y = 2, overlap = 10)
#'
#' # Plot one of the tiles
#' plot(tiles[[1]])
#' }
#'
#' @export
#'
split_raster <- function(rast, n_x = 10, n_y = 10, overlap = 0) {
    # Ensure input is a SpatRaster
    if (!inherits(rast, "SpatRaster")) {
        stop("Input 'rast' must be a terra::SpatRaster object.") }

    # Get raster extent and resolution
    e <- terra::ext(rast)
    res <- terra::res(rast)[1]

    # Compute break points along x and y
    x_breaks <- seq(e$xmin, e$xmax, length.out = n_x + 1)
    y_breaks <- seq(e$ymin, e$ymax, length.out = n_y + 1)

    # Create list of extents
    ext_list <- list()
    tile_id <- 1
    for (i in 1:n_x) {
        for (j in 1:n_y) {
            # Base extent
            ext_i <- terra::ext(x_breaks[i], x_breaks[i + 1], y_breaks[j], y_breaks[j + 1])
            # Optional overlap (in same units as raster CRS)
            if (overlap > 0) {
                ext_i <- terra::extend(ext_i, overlap)
            }
            ext_list[[tile_id]] <- ext_i
            tile_id <- tile_id + 1
            }
        }
    # Crop the raster into tiles
    tiles <- lapply(ext_list, function(ext_i) terra::crop(rast, ext_i))
    # Assign names for clarity
    names(tiles) <- sprintf("tile_%03d", seq_along(tiles))
    return(tiles)
}
