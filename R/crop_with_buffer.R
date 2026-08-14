#' Crop a Raster with a Buffer around a Vector Layer
#'
#' Crops a `SpatRaster` object using the extent of a `SpatVector` object,
#' optionally adding a buffer around the vector extent before cropping.
#'
#' @param raster A `SpatRaster` object to be cropped.
#' @param vector A `SpatVector` object used to define the cropping extent.
#' @param buffer A numeric value specifying the buffer used to expand the cropping
#'        extent, expressed in the map units of `vector`. For a lon-lat layer the
#'        buffer is in degrees; for a projected layer it is in the units of that
#'        projection, typically meters. Default is 0 (no buffer).
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param ... Additional arguments passed to [`terra::crop()`].
#'
#' @return A cropped `SpatRaster` object based on the buffered extent of the input vector layer.
#'
#' @details
#' The function computes the bounding box of the input vector and expands it in all directions
#' by the buffer amount. The raster is then cropped to this expanded extent.
#'
#' No reprojection is performed, so `buffer` is always interpreted in the map
#' units of the data. To buffer by a metric distance on a lon-lat layer, project
#' `raster` and `vector` to a suitable CRS first, for example with [get_utm_crs()].
#'
#' @seealso [terra::crop]
#' @export
#'
#' @examples
#' \dontrun{
#' r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
#' v <- terra::vect(system.file("ex/lux.shp", package = "terra"))
#' cropped <- crop_with_buffer(r, v, buffer = 500)
#' terra::plot(cropped)
#' }
crop_with_buffer <- function(raster, vector, iteration = NULL,
                             buffer = 0, ...) {
    if (!is.null(iteration)) cat("Cropping:", iteration, "\n")

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
