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


