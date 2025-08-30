#' Crop a Raster with a Buffer around a Vector Layer
#'
#' Crops a `SpatRaster` object using the extent of a `SpatVector` object,
#' optionally adding a buffer around the vector extent before cropping.
#'
#' @param raster A `SpatRaster` object to be cropped.
#' @param vector A `SpatVector` object used to define the cropping extent.
#' @param buffer A numeric value specifying the buffer (in map units) to expand the cropping extent.
#'        Default is 0 (no buffer).
#' @param ... Additional arguments passed to [`terra::crop()`].
#'
#' @return A cropped `SpatRaster` object based on the buffered extent of the input vector layer.
#'
#' @details
#' The function computes the bounding box of the input vector and expands it in all directions
#' by the buffer amount. The raster is then cropped to this expanded extent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
#' v <- terra::vect(system.file("ex/lux.shp", package = "terra"))
#' cropped <- crop_with_buffer(r, v, buffer = 500)
#' terra::plot(cropped)
#' }
crop_with_buffer <- function(raster, vector, buffer = 0, buffer_unit = "native", ...) {
    if (!inherits(raster, "SpatRaster")) stop("raster must be a SpatRaster")
    if (!inherits(vector, "SpatVector")) stop("vector must be a SpatVector")

    # If buffer is in meters and data is lon-lat, project first
    if (buffer_unit == "meters" && is.lonlat(raster)) {
        proj_crs <- get_utm_crs(vector)
        raster_proj <- terra::project(raster, proj_crs)
        vector_proj <- terra::project(vector, proj_crs)
    } else { # same unit of measure
        raster_proj <- raster
        vector_proj <- vector
    }

    # Compute buffered extent
    vec_ext <- terra::ext(vector)
    buffered_ext <- terra::ext(
        terra::xmin(vec_ext) - buffer,
        terra::xmax(vec_ext) + buffer,
        terra::ymin(vec_ext) - buffer,
        terra::ymax(vec_ext) + buffer
    )

    # Crop the raster using the buffered extent
    cropped_raster <- terra::crop(raster, buffered_ext, ...)
    return(cropped_raster)
}
