#' Merge survey observations with other data.
#'
#' The two tables are matched on the column identifying the units.
#'
#' @param survey A data frame. It must contain the unit identifier column used
#'   for the merging.
#' @param id Optional character string naming the column that identifies the
#'   units. When `NULL`, `ID` is used, or `ID_adm_div` when that is the only
#'   one present.
#' @param iteration optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param new_value A data frame. The data to be merge with the
#'   survey. It must carry the same identifier column as `survey`.
#'
#' @returns A \link[tibble]{tbl_df}, with variables from the survey and new
#'   weather values.
#'
#' @seealso [dplyr::full_join()]
#'
#' @export
#'
#' @examples
#' tmax <- terra::rast(
#'   system.file("extdata", "tmax_daily.tif", package = "climatic4economist"))
#' survey <- read.csv(
#'   system.file("extdata", "survey.csv", package = "climatic4economist"))
#' coord <- prepare_coord(survey, lat_var = lat, lon_var = lon)
#' points <- georef_coord(coord, geom = c("lon", "lat"), crs = "EPSG:4326")
#' tmax_points <- extract_by_coord(tmax[[1:5]], points)
#'
#' merge_with_survey(coord, tmax_points)

merge_with_survey <- function(survey,
                              new_value,
                              iteration = NULL,
                              id = NULL) {
    if (!is.null(iteration)) cat("merging with survey:", iteration, "\n")

    key <- resolve_key(survey, id)
    if (!key %in% names(new_value)) {
        stop("Column `", key, "` not found in `new_value`. Both tables must ",
             "carry the same identifier column.", call. = FALSE)
    }

    survey |>
        dplyr::select(-dplyr::matches(date_pattern())) |>
        dplyr::full_join(new_value, by = key, relationship = "many-to-many") |>
        tibble::as_tibble()
}
