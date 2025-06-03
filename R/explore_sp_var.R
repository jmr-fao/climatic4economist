# This code does:
#   1)   explores the properties of some spatial data


# ==== Set Up ========================= ----
path_to_soil <- file.path("..", "data", "spatial", "AgroEcological", "soil",
                         "GAEZv5", "GAEZ-V5.HWSDV201.tif")


# ==== Read =========================== ----
soil <- terra::rast(path_tosoil)


terra::plot(soil)










#####
####
