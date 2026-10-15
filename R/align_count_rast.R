#' Align a Count Raster onto the Grid of Another Raster
#'
#' Resamples a raster of counts (people, livestock heads) onto the grid of
#' `template` so that it can serve as extraction weights for `template`, for
#' example with [extract_weighted_by_poly()].
#'
#' @details
#' A count cannot be interpolated like a continuous value. The method follows
#' the resolutions:
#' * `template` coarser than `count`: the counts are summed into each template
#'   cell, so totals are preserved. When the grids nest (the resolution ratio
#'   is a whole number and the cell edges line up, e.g. 30 arc-seconds into
#'   5 arc-minutes) the sum is exact, with [terra::aggregate()]; otherwise
#'   [terra::resample()] with `method = "sum"` shares each count by overlap.
#' * `template` as fine as or finer than `count`: each template cell takes the
#'   count of the cell it falls in (`method = "near"`). Totals are not
#'   preserved, but the relative weights between cells are, which is what a
#'   weighting needs.
#'
#' `NA` counts are set to 0. Weights must have no `NA`, because
#' [exactextractr::exact_extract()] returns `NaN` for a polygon when any of its
#' weight cells is `NA`.
#'
#' @param count A `SpatRaster` of counts, in the same CRS as `template`.
#' @param template A `SpatRaster` whose grid the counts are put on.
#' @param tolerance Relative margin below which the two resolutions are
#'   treated as equal. Default `0.01`.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#'
#' @return A `SpatRaster` on the grid of `template`, with no `NA`.
#'
#' @seealso [extract_weighted_by_poly()], [terra::resample()]
#' @export
#'
#' @examples
#' pop <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.1,
#'                    crs = "epsg:4326", vals = 1)
#' coarse <- terra::rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.5,
#'                       crs = "epsg:4326", vals = 0)
#' align_count_rast(pop, coarse) # 25 people in each coarse cell
align_count_rast <- function(count, template, tolerance = 0.01,
                             iteration = NULL) {
    if (!is.null(iteration)) cat("Aligning:", iteration, "\n")

    checkmate::assert_class(count, "SpatRaster")
    checkmate::assert_class(template, "SpatRaster")
    checkmate::assert_number(tolerance, lower = 0)

    coarser <- terra::res(template)[1] > terra::res(count)[1] * (1 + tolerance)

    if (!coarser) {
        aligned <- terra::resample(count, template, method = "near")
    } else if (is_nested(count, template, tolerance)) {
        ## cut to the template's extent first, so that the blocks summed start
        ## at the template's corner
        fact <- round(terra::res(template) / terra::res(count))
        aligned <- count |>
            terra::extend(terra::ext(template), snap = "near") |>
            terra::crop(terra::ext(template), snap = "near") |>
            terra::aggregate(fact = fact, fun = "sum", na.rm = TRUE) |>
            terra::resample(template, method = "near")
    } else {
        aligned <- terra::resample(count, template, method = "sum")
    }

    terra::classify(aligned, cbind(NA, 0))
}

## TRUE when every cell of `template` is made of whole cells of `count`: a
## whole-number resolution ratio and cell edges that line up
is_nested <- function(count, template, tolerance) {
    ratio <- terra::res(template) / terra::res(count)
    whole_ratio <- all(abs(ratio - round(ratio)) < tolerance)

    ## distance of the template's corner from the count grid, in count cells
    offset <- (c(terra::xmin(template), terra::ymax(template)) -
                   c(terra::xmin(count), terra::ymax(count))) / terra::res(count)
    edges_align <- all(abs(offset - round(offset)) < tolerance)

    whole_ratio && edges_align
}
