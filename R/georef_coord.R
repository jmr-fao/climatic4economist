#' Georeference based on coordinates and coordinate reference system
#'
#' This this function transform a data frame into a point
#' \link[terra]{SpatVector}.
#'
#' @param df A data frame.
#' @param geom character. The field name(s) with the geometry data. Two names
#'   for x and y coordinates of points.
#' @param id Optional character string naming the column that identifies the
#'   units. When `NULL`, `ID` is used, or `ID_adm_div` when that is the only
#'   one present.
#' @param crs character. The coordinate reference system in one of the following
#'   formats: WKT/WKT2, <authority>:<code>, or PROJ-string notation. See
#'   \link[terra]{crs}.
#'
#' @returns A \link[terra]{SpatVector} of points with one variable `ID` as
#'   location unique identifier.
#'
#' @export
#'
#' @seealso [terra::vect]
#'
#' @examples
#' georef_coord(survey, crs = "epsg:4326")

georef_coord <- function(df, geom, crs, id = NULL) {
    key <- resolve_key(df, id)

    df |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(key)),
                        dplyr::pick({{geom}})) |>
        terra::vect(crs = crs, geom = geom)
}
