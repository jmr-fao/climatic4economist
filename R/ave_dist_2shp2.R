ave_dist_2shp2 <- function(country_ext, target, shape_input, UTM, res_raster, name_dist) {

  # Verifying that input, target, and extension are correct
  args <- list(country_ext, target, shape_input)
  arg_names <- as.list(substitute(list(country_ext, target, shape_input)))[-1]

  for (i in seq_along(args)) {
    obj <- args[[i]]
    name <- deparse(arg_names[[i]])  # Get name

    # Check if object is an sf data frame
    if (!inherits(obj, "sf")) {
      stop(paste(name, "is not an sf dataframe."))
    }

    # Ensure the CRS is defined and is exactly EPSG:4326
    crs <- st_crs(obj)
    if (is.null(crs) || is.na(crs$epsg) || crs$epsg != 4326) {
      stop(paste(name, "does not have WGS84 (EPSG:4326) coordinate system."))
    }
  }

  # Detect whether UTM is defined
  use_geodesic <- missing(UTM)

  # Convert to UTM if defined; otherwise, stay in WGS84
  if (!use_geodesic) {

    message(paste("Using UTM zone:", UTM))

    utm_country <- paste("+proj=utm +zone=", UTM, " +datum=WGS84 +units=m", sep="")

    country_ext_utm <- st_transform(country_ext, utm_country)
    target_utm <- st_transform(target, utm_country)
    shape_utm <- st_transform(shape_input, utm_country)

    # Create raster and compute Euclidean distance
    #create the empty shape_input raster and assigning with points to calculate distance from shape(roads, etc)
    shape_utm_coords <- st_coordinates(shape_utm)
    spatial_points <- SpatialPoints(shape_utm_coords[, c("X", "Y")], proj4string = CRS(st_crs(shape_utm)$proj4string))
    crs(spatial_points) <- st_crs(shape_utm)$proj4string #just defines crs

    e <- extent(country_ext_utm) #instead of using ext, I define borders a bit larger to have all country border and a bit more
    r <- raster(xmn = e@xmin - 10000, xmx = e@xmax + 10000,
                ymn = e@ymin - 10000, ymx = e@ymax + 10000,
                res = res_raster)
    crs(r) <- st_crs(shape_utm)$proj4string

    # Compute Euclidean distance in meters
    d_r_utm <- distanceFromPoints(r, spatial_points)

    # Convert resolution from meters to degrees
    center_lat <- terra::ext(d_r_utm)[3] + (terra::ext(d_r_utm)[4] - terra::ext(d_r_utm)[3]) / 2

    #At the equator, 1 degree ≈ 111.32 km (111,320 meters). UTM
    meters_per_degree <- 111320 * cos(center_lat * pi / 180)  # Adjusted for latitude
    res_degrees <- abs(res_raster / meters_per_degree)

    #RESAMPLING: project-CRS and rescaling to shape crs. terra::resample give same results, but does not project
    d_r <- terra::project(terra::rast(d_r_utm), crs(shape_input),
                          method="bilinear",res=res_degrees) #using res_degrees computed before

  } else {

    message("UTM is missing. Using geodesic distances (WGS84 EPSG:4326).")

    # Stay in WGS84 and compute geodesic distances
    country_ext_wgs84 <- st_as_sf(country_ext)
    target_wgs84 <- st_as_sf(target)
    shape_wgs84 <- st_as_sf(shape_input)

    # Create raster with expanded extent using res_raster (in degrees)
    e <- extent(country_ext_wgs84)
    res_degrees_wgs84 <-  res_raster / 111320  # Convert resolution from meters to degrees around the equator
    r <- raster(xmn = e@xmin - 1, xmx = e@xmax + 1, #Expanding by 1 degree.
                ymn = e@ymin - 1, ymx = e@ymax + 1,
                res = res_degrees_wgs84)
    crs(r) <- "+proj=longlat +datum=WGS84"

    # Compute geodesic distances
    shape_points <- terra::vect(shape_wgs84)
    shape_points <- terra::centroids(shape_points)  # Get centroid points of shape_input
    d_r <- terra::distance(terra::rast(r), shape_points)

  }

  ### ### ### ### ### ### ### ### ### ### ### ###
  # Calculate average distance of target to shape raster
  ### ### ### ### ### ### ### ### ### ### ### ###

  # Preparing objects for using extract
  target_extract <- st_as_sf(target)
  d_r_extract <- d_r |> as("SpatRaster")

  # Extract mean distance per region
  dr <- d_r_extract |>
    terra::extract(target_extract,
                   weights = TRUE,
                   fun = "mean", bind = TRUE,
                   touches = TRUE,  # if weights=TRUE, touches=TRUE by default
                   na.rm = TRUE) |> sf::st_as_sf()

  dr_data <- as.data.frame(dr) |> dplyr::select(-"geometry")
  setnames(dr_data, old = c("layer"), new = c(paste("ave_dist_", name_dist, sep = "")))

  return(dr_data)
}




