#' Compute percentiles and return as data frame
#'
#' This function calculates the specified percentiles of a numeric vector and
#' returns the result as a tidy dataframe.
#'
#' @param x A numeric vector for which percentiles are computed.
#' @param p A numeric vector specifying the probabilities at which percentiles
#' should be computed (values between 0 and 1).
#'
#' @return A tibble (data frame) with the computed percentiles, where column
#' names are derived from the probability values (e.g., `25p`, `50p`, `75p`).
#'
#' @export
#'
#' @seealso [quantile_na_check]

quantile_df <- function(x, p, replace) {
    quantile_na_check(x, p, replace) |>
        tibble::enframe() |>
        tidyr::pivot_wider(names_from = name, values_from = value) |>
        dplyr::rename_with(~gsub("%", "p", .x))
}
