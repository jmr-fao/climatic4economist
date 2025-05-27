#' Extract and Aggregate Raster Values by Polygons
#'
#' This function extracts raster values within polygon features and aggregates them
#' using a specified summary function (e.g., "mean", "sum", "median"). It returns
#' the extracted values as a tibble.
#'
#' @param raster A `SpatRaster` object from the \code{terra} package.
#' @param poly A `SpatVector` object representing polygon features.
#' @param fn_agg A function name specifying how to aggregate raster values within each polygon.
#'   Default is `mean`.
#'
#' @return A `tibble` with aggregated raster values, one row per polygon.
#'
#' @seealso \code{\link[terra]{extract}}
#'
#' @export
#'
#' @examples
#' r <- terra::rast(nrows=10, ncols=10)
#' terra::values(r) <- runif(ncell(r))
#' p <- vterra::ect(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE), type="polygons")
#'
#' extract_by_poly(r, p)

extract_by_poly <- function(raster, poly, fn_agg = mean, categorical = FALSE, na_rm = TRUE) {
    if (categorical) {
        ext <- terra::extract(raster, poly, bind = TRUE) |>
            terra::values() |>
            dplyr::as_tibble(ext_tbl) |>
            dplyr::group_by(ID, dplyr::across(-ID)) |>
            dplyr::summarise(count = dplyr::n(), .groups = "drop")

    } else {
        terra::extract(raster,
                       poly,
                       fun = fn_agg,
                       bind = TRUE,
                       na.rm = na_rm) |>
        terra::values() |>
        dplyr::as_tibble()
    }
}
