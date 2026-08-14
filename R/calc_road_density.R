#' Compute Road Network Density per Polygon
#'
#' Computes road network density (length per unit area) for each polygon
#' by intersecting a line road network with polygon geometries.
#'
#' @param polygons A `terra::SpatVector` polygon object.
#' @param roads A `terra::SpatVector` line object representing roads.
#' @param poly_id Optional character string specifying the polygon ID column.
#'   If NULL, the function searches for `"ID"` or `"ID_Adm_div"`.
#' @param return_geometry Logical. If TRUE, returns a `SpatVector` with geometries.
#'   If FALSE (default), returns a tibble with polygon attributes only.
#'
#' @return Either a [terra::SpatVector] or a [tibble::tbl_df] with columns:
#'   \describe{
#'     \item{road_length_km}{Total road length inside each polygon in km}
#'     \item{n_roads}{Number of road segments intersecting each polygon}
#'     \item{area_km2}{Polygon area in km²}
#'     \item{road_density}{Road length per km²}
#'   }
#'
#' @seealso [terra::intersect] [terra::perim]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' polygons <- terra::vect("admin.shp")
#' roads    <- terra::vect("roads.shp")
#'
#' # Return tibble (default)
#' result_tbl <- calc_road_density(polygons, roads)
#'
#' # Return SpatVector with geometries
#' result_vect <- calc_road_density(polygons, roads, return_geometry = TRUE)
#' }

calc_road_density <- function(polygons, roads, poly_id = NULL, return_geometry = FALSE) {

    # check CRS
    if (!terra::same.crs(polygons, roads)) {
        warning("CRS of polygons and roads do not match. Roads are reprojected to match polygons")
        roads <- terra::project(roads, terra::crs(polygons))
    }

    # find polygon ID
    if (is.null(poly_id)) {
        found_ids <- grep("^(ID|ID_Adm_div)$", names(polygons), value = TRUE)
        if (length(found_ids) == 0) stop("No valid ID column found. Provide `poly_id` explicitly.")
        poly_id <- found_ids[1]
    }

    # intersect roads with polygons
    roads_split <- terra::intersect(roads, polygons) |>
        tidyterra::select(dplyr::all_of(poly_id))

    # if there are no intersection return 0
    if (nrow(roads_split) == 0) {
        warning("No intersections found.")
        polygons <- polygons |>
            tidyterra::mutate(road_length_km = 0,
                              n_roads        = 0,
                              area_km2       = terra::expanse(polygons, unit = "km"),
                              road_density   = 0)

        if (!return_geometry) return(dplyr::as_tibble(polygons))
        return(polygons)
    }

    # compute road length
    roads_split <- roads_split |>
        tidyterra::mutate(road_length_km = terra::perim(roads_split) / 1000)

    # aggregate by polygon ID
    road_sum <- roads_split |>
        terra::aggregate(by = poly_id, fun = sum, na.rm = TRUE, count = TRUE) |>
        terra::values() |>
        dplyr::rename(road_length_km = agg_road_length_km,
                      n_roads = agg_n)

    # merge back to polygons
    polygons <- polygons |>
        tidyterra::full_join(road_sum, by = poly_id) |>
        tidyterra::mutate(
            road_length_km = dplyr::if_else(is.na(road_length_km), 0, road_length_km),
            n_roads        = dplyr::if_else(is.na(n_roads), 0, n_roads),
            area_km2       = terra::expanse(polygons, unit = "km"),
            road_density   = road_length_km / area_km2)

    # return as tibble or SpatVector
    if (!return_geometry) polygons <- dplyr::as_tibble(polygons)
    polygons
}
