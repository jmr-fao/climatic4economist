#' Get UTM CRS for a SpatVector (averaging centroids if multiple features)
#'
#' Computes the appropriate UTM coordinate reference system (CRS) based on
#' the average centroid coordinates of the input `SpatVector`.
#' Assumes the input vector is in geographic coordinates (longitude/latitude).
#'
#' @param vector A `SpatVector` object with geographic coordinates (lon-lat).
#'
#' @return A `terra::crs` object representing the UTM zone for the averaged centroid.
#'
#' @details
#' The function calculates the average longitude and latitude of all feature
#' centroids, determines the UTM zone and hemisphere, and returns a `terra::crs`
#' object corresponding to the WGS84 UTM projection.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' v <- terra::vect(system.file("ex/lux.shp", package = "terra"))
#' utm_crs <- get_utm_crs(v)
#' print(utm_crs)
#' }

get_utm_crs <- function(vector) {
    if (!inherits(vector, "SpatVector")) stop("Input must be a SpatVector")

    # Extract centroids and average coordinates
    centroids_df <- terra::crds(terra::centroids(vector), df = TRUE)
    avg_lon <- mean(centroids_df$x)
    avg_lat <- mean(centroids_df$y)

    # Compute UTM zone
    utm_zone <- floor((avg_lon + 180) / 6) + 1
    hemisphere <- ifelse(avg_lat < 0, "+south", "")

    # Construct PROJ string
    crs_string <- paste(
        "+proj=utm",
        paste0("+zone=", utm_zone),
        hemisphere,
        "+datum=WGS84 +units=m +no_defs"
    )

    # Return as terra crs object
    terra::crs(crs_string)
}
