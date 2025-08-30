#' Extract Polygon Attributes for Point Locations
#'
#'
#' This function intersects point coordinates with a polygon layer and returns polygon attributes
#' for each point. If a point falls outside all polygons, the function assigns attributes from
#' the nearest polygon.
#'
#' @param point A data frame or `SpatVector` of point features. If a data frame, it must contain
#'        columns for longitude and latitude specified in `geom`.
#' @param poly A `SpatVector` object representing polygons with attributes to extract.
#' @param iteraction Optional. A label (e.g., an ID or name) printed during execution to indicate
#'        which subset is being processed. Useful when running the function in a loop or batch.
#' @param geom A character vector of length two specifying the names of the longitude and
#'        latitude columns (default is `c("lon", "lat")`).
#' @param crs A character string specifying the CRS of the input coordinates (default is `"epsg:4326"`).
#'
#' @return A tibble containing the original point data and the corresponding polygon attributes.
#'         For points outside all polygons, attributes are taken from the nearest polygon.
#'
#' @details
#' The function performs an exact spatial intersection. If no intersection is found,
#' it finds the nearest polygon using `sf::st_nearest_feature()` and merges its attributes.
#
#' @export

get_poly_attr_for_point <- function(point,
                                    poly,
                                    iteraction = NULL,
                                    geom = c("lon", "lat"),
                                    crs = "epsg:4326",
                                    outside = TRUE) {
    if (!is.null(iteraction)) cat("processing:", iteraction, "\n")
    if (!inherits(point, "SpatVector")) {
        point <- terra::vect(point, geom = geom, crs = crs, keepgeom = TRUE)
    } else {
        coord <- terra::crds(point)
        point$lon <- coord[,1]
        point$lat <- coord[,2]
    }

    # Find intersecting attributes
    attrs_in <- terra::intersect(point, poly) |>
        terra::values()

    # Identify points with no match
    comm_var <- find_merge_var(point, attrs_in)
    point_out <- dplyr::anti_join(terra::values(point), attrs_in, by = comm_var)

    # For unmatched points, assign nearest polygon attributes
    if (nrow(point_out) > 0 & outside) {
        cat("Some points are outside the polygons. They are assigned to the closest polygon.\n")
        point_out_id <- point_out |>
            dplyr::select(setdiff(comm_var, geom))
        cat("The points are:", unlist(point_out_id))

        sf_points <- point_out |>
            terra::vect(geom = geom, crs = crs, keepgeom = TRUE) |>
            sf::st_as_sf()
        sf_polygons <- sf::st_as_sf(poly)

        nearest_idx <- sf::st_nearest_feature(sf_points, sf_polygons)
        nearest_attrs <- sf_polygons[nearest_idx, ] |>
            sf::st_drop_geometry()

        attrs_out <- sf_points |>
            sf::st_drop_geometry() |>
            cbind(nearest_attrs)

        out <- rbind(attrs_in, attrs_out)
    } else {
        out <- attrs_in
    }

    dplyr::as_tibble(out)
}
