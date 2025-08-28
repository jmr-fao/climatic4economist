#' WRITTEN BY LUIS BECERRA ON 28/06/2024. Updated on 14/03/2025
#' This function calculates the average distance (in m) of each `ID` element in `target` to the object `shape_input`
#' It uses distanceFromPoints which works better, faster. st_distance function yields weird results
#' Works better for UTM now
#'
#' @param UTM corresponds to the country's location in the UTM coordinate system.
#' As for calculating distances it is more convenient to use meters instead of degrees,
#' and the coordinate system needs to be transformed into the UTM of the country, which
#' is more precise around the equator.
#' Otherwise, it uses the default EPSG:3857 (Web Mercator),
#' "+proj=merc +datum=WGS84 +units=m" which holds at the global level
#'
#' @param country_ext defines the whole extension of the country, the country borders. It could use GADM as extension
#'
#' @param target is the shapefile (adm-div such as communes, villages or HH coordinates) for which the
#' average distance will be calculated.
#'
#' @param shape_input is the shapefile for which a raster of distances will be calculated (eg. roads, rivers, etc)
#'
#' `target`, `shape_input` and `country_ext` have to be in "sf-data.frame" in WGS84
#'
#' @param res_raster is the resolution in meters for the raster of distances that will be created.
#' A higher resolution increase the time of processing
#'
#' @param name_dist name of the average distance variable constructed. It will be
#' created as ave_dist_NAME with name_dist="NAME"
#'
#' @return A \link[base]{data.frame}, with the average distance variable calculated for each `ID`
#'
#' @seealso [raster::distanceFromPoints()]
#'
#' @export
#'
#' @examples
#' ave_dist_rivers <- ave_dist_2shp(country_ext=gadm3,target=polygons_gns,shape_input=rivers_fao, UTM=31,res_raster=10000,name_dist="rivers")
#'

ave_dist_2shp <- function(country_ext, target, shape_input, UTM, res_raster, name_dist) {

  #verifying that input, target and extension are correct
  args <- list(country_ext,target,shape_input)
  arg_names <- as.list(substitute(list(country_ext,target,shape_input)))[-1]

  for (i in seq_along(args)) {
    obj <- args[[i]]
    name <- deparse(arg_names[[i]])  # Get  name

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

  # message("All inputs are valid sf data frames in WGS84 coordinate system.")
  # return(TRUE)

  if (missing(UTM)) {
    print("Argument 'UTM' is missing. Defining Global default EPSG:3857 (Web Mercator)")
    utm_country<-"+proj=merc +datum=WGS84 +units=m"

  } else {
    print(paste("UTM is:", UTM))
    utm_country<-paste("+proj=utm +zone=", UTM," +datum=WGS84 +units=m",sep="")
  }

  country_ext_utm <- st_transform(country_ext, utm_country)
  target_utm <- st_transform(target, utm_country)
  shape_utm <- st_transform(shape_input, utm_country)

  #create the empty shape_input raster and assigning with points to calculate distance from shape(roads, etc)
  shape_utm_coords <- st_coordinates(shape_utm)
  spatial_points <- SpatialPoints(shape_utm_coords[, c("X", "Y")], proj4string = CRS(st_crs(shape_utm)$proj4string))
  crs(spatial_points) <- st_crs(shape_utm)$proj4string #just defines crs

  e<-extent(country_ext_utm) #instead of using ext, I define borders a bit larger to have all country border and a bit more
  r <- raster(xmn=e@xmin-10000, xmx=e@xmax+10000,
              ymn=e@ymin-10000, ymx=e@ymax+10000,
              res = res_raster)
  crs(r) <- st_crs(shape_utm)$proj4string

  #calculating distance in meters
  d_r_utm <- distanceFromPoints(r, spatial_points)

  #equivalent resolution of UTM in degrees
  center_lat <- terra::ext(d_r_utm)[3] + (terra::ext(d_r_utm)[4] - terra::ext(d_r_utm)[3]) / 2  # Approx. center latitude

  if (missing(UTM)) {
    # As it is defined in meters
    # Use a fixed approximation for meters_per_degree in Web Mercator
    meters_per_degree <- 111320  # Approximation

  } else {
    #At the equator, 1 degree ≈ 111.32 km (111,320 meters). UTM
    meters_per_degree <- 111320 * cos(center_lat * pi / 180)  # Adjusted for latitude.
  }

  res_degrees <- abs(res_raster / meters_per_degree)

  #RESAMPLING: project-CRS and rescaling to shape crs. terra::resample give same results, but does not project
  d_r <- terra::project(terra::rast(d_r_utm), crs(shape_input),
                        method="bilinear",res=res_degrees) #using res_degrees computed before

  ### ### ### ### ### ### ### ### ### ### ### ###
  # Calculate average distance of target to shape raster
  ### ### ### ### ### ### ### ### ### ### ### ###

  #preparing objects for using extract
  target_extract<-st_as_sf(target)
  d_r_extract <- d_r|> as("SpatRaster")

  #terra::extract runs much faster and it's more efficient.
  dr <- d_r_extract |>
    terra::extract(target_extract,
                   weights = TRUE,
                   fun = "mean", bind=TRUE,
                   touches = TRUE, #if weights=TRUE by default touches=TRUE.
                   na.rm = TRUE) |> sf::st_as_sf()

  dr_data<- as.data.frame(dr) |>dplyr::select(-"geometry")

  setnames(dr_data,old=c("layer"),new=c(paste("ave_dist_", name_dist,sep="")))

  return(dr_data)

}

#' WRITTEN BY LUIS BECERRA ON 28/06/2024. Updated on 14/03/2025
#' This function calculates the average density of points for each `ID` element of `target`to the object `shape_input`
#'
#' @param country_ext defines the whole extension of the country, the country borders. It could use GADM as extension
#'
#' @param shape_input is the shapefile from which to calculate points density (eg. roads, rivers, etc)
#'
#' @param target is the shapefile (adm-div such as communes, villages or HH coordinates) for which the
#' average point density will be calculated.
#'
#' `target`, `shape_input` and `country_ext` have to be in "sf-data.frame" in WGS84
#'
#' @param res_raster is the resolution in degrees for the raster of distances that will be created and applied
#' to `shape_input' (0.1 could be equivalent to 10km or 0.1*100000=10000m )
#' A higher resolution increase the time of the process
#'
#' @param name_density name of the average density variable constructed. It will be
#' created as ave_den_NAME with name_density="NAME"
#'
#' @return A \link[base]{data.frame}, with the average density variable calculated for each `ID`

ave_point_density_2shp <- function(country_ext,target,shape_input,res_raster,name_density) {

  #verifying that input, target and extension are correct
  args <- list(country_ext,target,shape_input)
  arg_names <- as.list(substitute(list(country_ext,target,shape_input)))[-1]

  for (i in seq_along(args)) {
    obj <- args[[i]]
    name <- deparse(arg_names[[i]])  # Get  name

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

  ### ### ### ### ### ### ### ### ### ### ### ###
  # average distance raster or raster points density
  ### ### ### ### ### ### ### ### ### ### ### ###

  ext_target = raster::extent(country_ext)
  ext_target@xmin<-ext_target@xmin-0.5
  ext_target@xmax<-ext_target@xmax+0.5
  ext_target@ymin<-ext_target@ymin-0.5
  ext_target@ymax<-ext_target@ymax+0.5
  kernel_grid<-raster(ext = ext_target, resolution = res_raster, crs = crs(target)) #using target ext
  kernel_density<-rasterize(coordinates(as_Spatial(shape_input)), kernel_grid, fun='count', background = 0)


  ### ### ### ### ### ### ### ### ### ### ### ###
  # Calculate average density of target to point raster
  ### ### ### ### ### ### ### ### ### ### ### ###

  #preparing objects for using extract
  target_extract<-st_as_sf(target)
  den_extract <- kernel_density|> as("SpatRaster")

  #terra::extract runs much faster and it's more efficient.
  denr <- den_extract |>
    terra::extract(target_extract,
                   weights = TRUE,
                   fun = "mean", bind=TRUE,
                   touches = TRUE, #if weights=TRUE by default touches=TRUE. weights=T or F did not change drastically the result. See colSums values
                   na.rm = TRUE) |> sf::st_as_sf()

  denr_data<- as.data.frame(denr) |>dplyr::select(-"geometry")

  setnames(denr_data,old=c("layer"),new=c(paste("ave_den_", name_density,sep="")))

  return(denr_data)

}









