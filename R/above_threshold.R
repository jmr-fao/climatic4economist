#' Compute amount above a threshold
#'
#' @param x A dataframe containing time-series data.
#' @param threshold A numeric value specifying the threshold.
#'
#' @return A dataframe indicating the amount exceeding the giving threshold.
#'
#' @export

above_threshold <- function(x, threshold) {
    unit_abv <- paste0("unit_abv_", threshold)
    x |>
        dplyr::transmute(difference := value - threshold,
                         {{unit_abv}} := dplyr::if_else(difference < 0, 0, difference)) |>
        dplyr::select(-difference)
}
