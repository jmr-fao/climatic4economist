#' Compute Distances Between Spatial Objects (using purrr::partial)
#'
#' Computes distances from each feature in `x` to features in `y`.
#' Optionally reduces multiple distances per feature using a summary function.
#'
#' @param x A `terra::SpatVector` (points, lines, or polygons) for which distances will be computed.
#' @param y A `terra::SpatVector` containing target features.
#' @param fun_summ Optional summary function (e.g., `min`, `max`, `mean`).
#'   If provided, reduces distances per feature in `x` to a single value.
#'   Default is `min`. If `NULL`, the full distance matrix is returned.
#' @param fun_args Optional list of additional arguments to pass to `fun_summ`.
#' @param use_union Logical. If TRUE (default) and `fun_summ` is not NULL,
#'   `y` will be unioned to simplify geometry before computing distances.
#'   Union can improve performance for large overlapping features
#' @param ... Additional arguments passed to `terra::distance()` (e.g., `method = "haversine"`).
#' @param return_geometry Logical. If TRUE, returns a `SpatVector` with geometries.
#'   If FALSE (default), returns a tibble with polygon attributes only.
#'
#' @return If `fun_summ` is provided, returns either a [terra::SpatVector] or
#'   a [tibble::tbl_df] with the attributes of `x`
#'   plus a `distance` column (in km). If `fun_summ` is NULL, returns a numeric
#'   distance matrix where rows correspond to features in `x` and columns to features in `y`.
#'
#' @seealso [terra::distance] [cal_nearest_distance]
#'
#' @export
cal_distance <- function(x, y, fun_summ = min, fun_args = list(), use_union = TRUE, return_geometry = FALSE, ...) {

    # CRS check
    if (!terra::same.crs(x, y)) {
        message("CRS do not match. Reprojecting y to x CRS.")
        y <- terra::project(y, terra::crs(x))
    }

    # union if requested
    if (use_union && !is.null(fun_summ)) y <- terra::union(y)

    # compute distance matrix
    d <- terra::distance(x, y, unit = "km", ...)

    # apply summary function if provided
    if (!is.null(fun_summ)) {
        # create a partial function with pre-bound arguments
        fun <- purrr::partial(fun_summ, !!!fun_args)

        d <- apply(d, 1, fun)

        out <- x |>
            tidyterra::mutate(distance = d)

        # return as tibble or SpatVector
        if (!return_geometry) {
            out <- out |>
                terra::values() |>
                dplyr::as_tibble()
        }
    } else {
        out <- d
    }

    return(out)
}
