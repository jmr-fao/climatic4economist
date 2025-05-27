#' Merge survey observations with other data.
#'
#' The functions uses the variable `ID` as the key matching variable.
#'
#' @param survey A data frame. It must contains an `ID` for the merging.
#' @param iteracation optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param new_value A data frame. The data to be merge with the
#'   survey. It must contains an `ID` variable for the merging.
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
                              iteracation = NULL) {
    if (!is.null(iteracation)) cat("meging with survey:", iteracation, "\n")

    survey |>
        dplyr::select(-dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::full_join(new_value, by = "ID", relationship = "many-to-many") |>
        tibble::as_tibble()
}
