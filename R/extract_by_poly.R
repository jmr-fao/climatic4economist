#' Extract and Aggregate Raster Values by Polygons
#'
#' This function extracts raster values within polygon features and aggregates them
#' using a specified summary function (e.g., "mean", "sum", "median"). It returns
#' the extracted values as a tibble.
#'
#' @details
#' The function automatically selects the processing engine based on the number
#' of features to balance speed and precision. For more than 1,000 polygons,
#' it defaults to \code{exactextractr}, which is faster for large datasets and
#' accounts for partial pixel coverage. For smaller datasets, it uses \code{terra}.
#'
#' @param raster A \code{SpatRaster} object from the \code{terra} package.
#' @param poly A \code{SpatVector} or \code{sf} object representing polygon features.
#' @param fn_agg A function or character string specifying how to aggregate
#'   raster values within each polygon. Default is \code{mean}.
#' @param na_rm Logical. If \code{TRUE}, \code{NA} values in the raster are
#'   ignored during aggregation. Default is \code{TRUE}.
#' @param pkg Optional character string. The package to use: \code{"terra"} or
#'   \code{"exactextractr"}. If \code{NULL}, a package is chosen based on polygon count.
#'   Default is \code{"terra"}.
#'
#' @return A \code{tibble} with aggregated raster values, one row per polygon.
#'
#' @seealso \code{\link[terra]{extract}}, \code{\link[exactextractr]{exact_extract}}
#'
#' @export
#'
#' @examples
#' r <- terra::rast(nrows=10, ncols=10)
#' terra::values(r) <- runif(ncell(r))
#' p <- terra::vect(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE), type="polygons")
#'
#' extract_by_poly(r, p)

extract_by_poly <- function(raster, poly, fn_agg = "mean", na_rm = TRUE, pkg = "terra") {

    if (is.null(pkg)) {
        pkg <-  dplyr::if_else(nrow(poly) > 1000, "exactextractr", "terra")
    }
    pkg <- match.arg(pkg, c("exactextractr", "terra"))
    if (pkg == "terra") {
        extract_values <- terra::extract(raster,
                                         poly,
                                         fun = fn_agg,
                                         bind = TRUE,
                                         na.rm = na_rm) |>
            terra::values() |>
            dplyr::as_tibble()
    }
    if (pkg == "exactextractr") {
        extract_values <- exactextractr::exact_extract(raster,
                                                       sf::st_as_sf(poly),
                                                       fun = fn_agg,
                                                       append_cols = names(poly),
                                                       force_df = TRUE) |>
            dplyr::rename_with(\(x) gsub(paste0("^", fn_agg, "\\."), "", x)) |>
            dplyr::as_tibble()
    }
    return(extract_values)

}
