# This code does:
#   1)  extract administrative borders for selected countries form global file
#   2)  save the extracted administrative borders in a specific folder for
#       selected country

# ===== Path to file ================== ----
path_to_files <- file.path("..", "..", "data", "adm_div", "GAUL")

# ==== Read =========================== ----
## list countiries                      ----
cntry <- list.files(path_to_files, pattern = "^[A-Z]{3}$")

## global lavel 1                       ----
global1 <- list.files(path_to_files, pattern = "global", full.names = TRUE) |>
    list.files(pattern = paste0("L", 1, "$"), full.names = TRUE) |>
    terra::vect()

## global level 2                       ----
global2 <- list.files(path_to_files, pattern = "global", full.names = TRUE) |>
    list.files(pattern = paste0("L", 2, "$"), full.names = TRUE) |>
    terra::vect()

# ==== Extract selected countries ===== ----
## country level 1                      ----
cntry_adm_div1 <- purrr::map(cntry,
                             \(x)  tidyterra::filter(global1, iso3_code == x)) |>
    setNames(cntry)

## country level 2                      ----
cntry_adm_div2 <- purrr::map(cntry,
                             \(x)  tidyterra::filter(global2, iso3_code == x)) |>
    setNames(cntry)

# ==== Write ========================== ----
## administrative level 1               ----
purrr::walk2(cntry, cntry_adm_div1,
             function(x, y) {
                 print(x)
                 file_name <- file.path(path_to_files, x,
                                        paste0("GAUL-", x, "-ADM1"),
                                        paste0("GAUL-", x, "-ADM1.geojson"))

                 terra::writeVector(y, filename = file_name, filetype = "geojson", overwrite = TRUE)})

## administrative level 2               ----
purrr::walk2(cntry, cntry_adm_div2,
            function(x, y) {
                 print(x)
                 file_name <- file.path(path_to_files, x,
                                        paste0("GAUL-", x, "-ADM2"),
                                        paste0("GAUL-", x, "-ADM2.geojson"))

                 terra::writeVector(y, filename = file_name, filetype = "geojson", overwrite = TRUE)})


#####
#####
