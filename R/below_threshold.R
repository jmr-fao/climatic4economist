#' Compute amount below a threshold
#'
#' @param x A dataframe containing time-series data.
#' @param threshold A numeric value specifying the threshold.
#'
#' @return A dataframe indicating the amount below the threshold.
#'
#' @export

below_threshold <- function(x, threshold) {
    unit_blw <- paste0("unit_blw_", threshold)
    x |>
        dplyr::transmute(difference := threshold - value,
                         {{unit_blw}} := dplyr::if_else(difference < 0, 0, difference)) |>
        dplyr::select(-difference)
}
