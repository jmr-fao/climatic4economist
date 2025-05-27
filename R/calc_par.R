#' Calculate Aggregated Parameters
#'
#' This function computes summary statistics for each unique ID by applying
#' a set of aggregation functions to the selected numeric columns.
#'
#' @param df A data frame containing at least an "ID" column and numeric columns
#'   with years as names.
#' @param agg_fns A function or a named list or a named vector of functions to
#'   be applied for aggregation.
#'
#' @return A data frame with aggregated statistics for each ID.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 2, 2, 3),
#'   `2001` = c(10, 15, 20, 25, 30),
#'   `2002` = c(5, 10, 15, 20, 25)
#' )
#' calc_par(df, list(mean = mean, sum = sum))
#'

calc_par <- function(df, pars, prefix = NULL) {
    df |>
        dplyr::select(
            dplyr::any_of(c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")),
            dplyr::matches("[0-9]{4}")) |>
        tidyr::pivot_longer(cols = dplyr::matches("[0-9]{4}")) |>
        dplyr::group_by(dplyr::pick(
            dplyr::any_of(c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")))) |>
        dplyr::summarise(dplyr::across(value, pars,
                                       .names = ifelse(is.null(prefix),
                                                       "{fn}",
                                                       paste0(prefix, "_{fn}"))),
                         .groups = "drop")
}
