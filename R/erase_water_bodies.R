#' Erase Large Water Bodies from Polygons
#'
#' Removes sea and lakes from polygons using a land/water mask, so that
#' statistics of the territory (elevation, cropland, emissions per km2) are
#' not diluted by water in shoreline or lake units.
#'
#' @details
#' The mask is cropped around the polygons, every cell that is not
#' `land_value` is turned into water polygons, and the water polygons of at
#' least `min_km2` are erased. Smaller patches stay part of the units: a
#' coarse mask only resolves large lakes anyway, and this drops single-cell
#' noise.
#'
#' With the GSHHS land/water mask of the spatial repository
#' (`geophysical/land_water_mask/GSHHS`, ~3 km), land is 100 and water 0.
#' At that resolution the mask can cut a few km off a coastline, so a very
#' small coastal unit can lose much of its area.
#'
#' A polygon entirely under water disappears from the result. Compare the row
#' counts, or match on an id, to find it.
#'
#' @param poly A `SpatVector` of polygons.
#' @param water A `SpatRaster` land/water mask, in the CRS of `poly`.
#' @param land_value Value of land cells in `water`; every other non-`NA`
#'   value is water. Default `100`, as in GSHHS.
#' @param min_km2 Smallest water body erased, km2. Default `50`.
#' @param buffer Buffer around `poly` when cropping `water`, in the map units
#'   of `poly`. Default `0.5`.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#'
#' @return `poly` with the water bodies erased, attributes kept.
#'
#' @seealso [terra::erase()], [crop_with_buffer()]
#' @export
#'
#' @examples
#' water <- terra::rast(xmin = 0, xmax = 2, ymin = 0, ymax = 2, res = 0.5,
#'                      crs = "epsg:4326", vals = 100)
#' water[1:2, 1:2] <- 0  # a water body in the top-left corner
#' p <- terra::vect(terra::ext(0, 2, 0, 2), crs = "epsg:4326")
#'
#' terra::expanse(p, unit = "km")
#' terra::expanse(erase_water_bodies(p, water), unit = "km")
erase_water_bodies <- function(poly, water, land_value = 100, min_km2 = 50,
                               buffer = 0.5, iteration = NULL) {
    if (!is.null(iteration)) cat("Erasing water bodies:", iteration, "\n")

    checkmate::assert_class(poly, "SpatVector")
    checkmate::assert_class(water, "SpatRaster")
    checkmate::assert_number(land_value)
    checkmate::assert_number(min_km2, lower = 0)

    water_poly <- water |>
        crop_with_buffer(poly, buffer = buffer) |>
        terra::classify(cbind(land_value, NA)) |>
        terra::as.polygons() |>
        terra::disagg()

    if (nrow(water_poly) == 0) return(poly)

    water_poly <- water_poly[terra::expanse(water_poly, unit = "km") >= min_km2]

    if (nrow(water_poly) == 0) return(poly)
    terra::erase(terra::makeValid(poly), water_poly)
}
