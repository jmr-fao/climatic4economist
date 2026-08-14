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
#' spi_survey <- merge_with_survey(survey, spi_values)

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
        dplyr::select(-dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::full_join(new_value, by = key, relationship = "many-to-many") |>
        tibble::as_tibble()
}
