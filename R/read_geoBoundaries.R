#' Read Sub National Administrative Divisions from geoBoundaries Files
#'
#' This function reads and processes administrative boundaries at at any given
#' level for one or more countries from a given directory path.
#' The function uses `terra` to load the vector files and `tidyterra` for cleaning
#' and renaming columns.
#' The data are obtained from [geoBoundaries](https://www.geoboundaries.org/countryDownloads.html).
#'
#' @param path_to_data A character string specifying the path where the country folders
#'        containing GeoJSON administrative division files are stored.
#' @param iso A character vector with one or more ISO 3-letter country codes (e.g., `"KEN"`, `"UGA"`).
#' @param lvl An integer specifying the desired level of administrative division to return:
#'        `1` for ADM1, or `2` for ADM2 (default).
#'
#' @return A named list of `SpatVector` objects, each corresponding to one country.
#'         - If `lvl = 1`, the function returns ADM1 polygons with columns: `adm_div_1`, `iso`, `lvl`.
#'         - If `lvl = 2`, it returns intersected ADM2 polygons including the corresponding ADM1 divisions,
#'           with columns: `iso`, `adm_div_1`, `adm_div_2`, `lvl`.
#'
#' @details
#' The function expects a directory structure like:
#' ```
#' path_to_data/
#' ├── KEN/
#' │   ├── .../KEN_ADM1.geojson
#' │   └── .../KEN_ADM2.geojson
#' ├── UGA/
#' │   ├── .../UGA_ADM1.geojson
#' │   └── .../UGA_ADM2.geojson
#' ```
#' It performs intersection between ADM2 and ADM1 shapes to ensure hierarchical alignment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "path/to/admin_data"
#' read_geoBoundaries(path_to_data = path, iso = c("KEN", "UGA"), lvl = 2)
#' }

read_geoBoundaries <- function(path_to_files, iso, lvl = 2, file_format = "geojson") {
    if (length(iso) > 1) iso <- paste(iso, collapse = "|")
    iso_name <- stringi::stri_split_fixed(iso, "|") |> unlist() |> sort()

    adm_div_1 <- list.files(path_to_files, pattern = iso, full.names = TRUE) |>
        lapply(list.files, full.names = TRUE) |>
        lapply(list.files, pattern = paste0("ADM1.", file_format), full.names = TRUE) |>
        lapply(terra::vect) |>
        lapply(tidyterra::rename,
               adm_div_1 = shapeName,
               iso = shapeGroup,
               lvl = shapeType) |>
        lapply(tidyterra::select, c(iso, adm_div_1)) |>
        setNames(iso_name)

    out <- lapply(adm_div_1,
                  dplyr::mutate,
                  ID_adm_div = as.character(1:dplyr::n()),
                  .before = 1)

    if (lvl == 2) {
        adm_div_1 <- lapply(adm_div_1, tidyterra::select, adm_div_1)
        adm_div_2_poly <- list.files(path_to_files, pattern = iso, full.names = TRUE) |>
            lapply(list.files, full.names = TRUE) |>
            lapply(list.files, pattern = paste0("ADM2.", file_format), full.names = TRUE) |>
            lapply(terra::vect) |>
            lapply(tidyterra::rename,
                   adm_div_2 = shapeName,
                   iso = shapeGroup) |>
            lapply(tidyterra::select, c(iso, adm_div_2)) |>
            setNames(iso_name)

        adm_div_2_info <- adm_div_2_poly |>
            purrr::map(terra::centroids, inside = TRUE) |>
            purrr::map2(adm_div_1,
                        terra::intersect) |>
            purrr::map(terra::values)

        adm_div_2 <- purrr::map2(adm_div_2_poly, adm_div_2_info, merge_by_common) |>
            purrr::map(tidyterra::relocate, iso, adm_div_1, adm_div_2)

        out <- lapply(adm_div_2,
                      dplyr::mutate,
                      ID_adm_div = as.character(1:dplyr::n()),
                      .before = 1)
    }

    adm_div_out <- out |>
        lapply(\(x) tidyterra::mutate(x,
                                      ID_adm_div = as.character(1:dplyr::n()),
                                      .before = 1))
    if (length(adm_div_out) == 1) return(adm_div_out[[1]])
    adm_div_out
}
