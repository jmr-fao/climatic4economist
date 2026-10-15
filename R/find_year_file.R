#' Find the File of a Yearly Raster Series for Given Years
#'
#' For a folder holding one raster per year (WorldPop, VIIRS, EDGAR, land
#' cover), returns the file of each requested year, or of its nearest
#' available year, with the year actually used.
#'
#' @details
#' The data year is read from each file name with `year_regex`. Name the year
#' by what surrounds it: many series also carry a release or processing year
#' in the name (EDGAR 2025 files are `EDGAR_2025_GHG_CO2_2015_...`), so a bare
#' `\\d{4}` would read the wrong one. Lookbehinds work well, e.g.
#' `"(?<=pop_)\\d{4}"` or `"(?<=CO2_)\\d{4}"`.
#'
#' A year the folder lacks takes the nearest year it has, the earlier one on
#' ties, unless it is more than `max_gap` years away; then `file` is `NA`.
#' Check `src_year` against `year` to see which values were filled.
#'
#' When a year exists both as `.tif` and `.tif.gz`, the `.tif` is used. A
#' year only available compressed gets the `/vsigzip/` prefix, so that
#' [terra::rast()] reads it without unzipping first (slowly: it decompresses
#' as it reads).
#'
#' @param path Folder of the series.
#' @param year Integer vector of requested years.
#' @param year_regex Regular expression matching the data year in the file
#'   names.
#' @param pattern Regular expression the file names must match. Default
#'   `"\\.tif(\\.gz)?$"`.
#' @param max_gap Largest distance in years accepted for a substitute year.
#'   Default `Inf`.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#'
#' @return A `tibble` with one row per element of `year`: `year`, `src_year`
#'   (the year used) and `file` (the path, ready for [terra::rast()]).
#'
#' @seealso [terra::rast()]
#' @export
#'
#' @examples
#' dir <- file.path(tempdir(), "series")
#' dir.create(dir, showWarnings = FALSE)
#' file.create(file.path(dir, paste0("pop_", 2015:2020, "_1km.tif")))
#'
#' find_year_file(dir, 2013:2016, year_regex = "(?<=pop_)\\d{4}")
find_year_file <- function(path, year, year_regex,
                           pattern = "\\.tif(\\.gz)?$", max_gap = Inf,
                           iteration = NULL) {
    if (!is.null(iteration)) cat("Finding year file:", iteration, "\n")

    checkmate::assert_directory_exists(path)
    checkmate::assert_integerish(year, any.missing = FALSE)
    checkmate::assert_string(year_regex)
    checkmate::assert_number(max_gap, lower = 0)

    files <- tibble::tibble(
        file = list.files(path, pattern = pattern, full.names = TRUE)
    ) |>
        dplyr::mutate(
            src_year = as.integer(stringr::str_extract(basename(file),
                                                       year_regex)),
            gz       = stringr::str_detect(file, "\\.gz$")
        ) |>
        dplyr::filter(!is.na(src_year)) |>
        dplyr::arrange(src_year, gz) |>
        dplyr::distinct(src_year, .keep_all = TRUE) |>
        dplyr::mutate(file = dplyr::if_else(gz, paste0("/vsigzip/", file),
                                            file))

    if (nrow(files) == 0) {
        stop("No file in `path` matches both `pattern` and `year_regex`.",
             call. = FALSE)
    }

    purrr::map(as.integer(year), \(y) {
        best <- files |>
            dplyr::arrange(abs(src_year - y), src_year) |>
            dplyr::slice(1)

        if (abs(best$src_year - y) > max_gap) {
            return(tibble::tibble(year = y, src_year = NA_integer_,
                                  file = NA_character_))
        }
        tibble::tibble(year = y, src_year = best$src_year, file = best$file)
    }) |>
        purrr::list_rbind()
}
