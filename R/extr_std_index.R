#' Extract Standardized Index Based on Thresholds
#'
#' This function computes standardized indices based on upper and lower thresholds.
#' If a value exceeds or falls below the respective threshold, it is classified accordingly.
#'
#' @param df A data frame containing the data to be standardized.
#' @param l_thrshld A list of lower threshold values. If NULL, lower threshold classification is skipped.
#' @param u_thrshld A list of upper threshold values. If NULL, upper threshold classification is skipped.
#'
#' @return A data frame containing the original data with additional standardized index columns.
#'
#' @export
#'
#' @examples
#' df <- data.frame(ID = 1:3, value = c(5, 10, 15))
#' l_thrshld <- list(8)
#' u_thrshld <- list(12)
#' extr_std_index(df, l_thrshld, u_thrshld)
#'

extr_std_index <- function(df, l_thrshld = NULL, u_thrshld = NULL) {

    if (!is.null(u_thrshld)) {
        std_abv <- purrr::map(u_thrshld, is_above, x = df) |>
            dplyr::bind_cols()
    } else {
        std_abv <- NULL
    }

    if (!is.null(l_thrshld)) {
        std_blw <- purrr::map(l_thrshld, is_below, x = df) |>
            dplyr::bind_cols()
    } else {
        std_blw <- NULL
    }

    list(df, std_abv, std_blw) |>
        purrr::compact() |>
        dplyr::bind_cols() |>
        dplyr::rename_with(~gsub("-", "", .x)) |>
        dplyr::rename_with(~gsub("day", "std", .x)) |>
        dplyr::rename(std_ind = value)
}
