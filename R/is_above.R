#' Identify values above a threshold
#'
#' @param x A dataframe containing time-series data.
#' @param threshold A numeric value specifying the threshold.
#'
#' @return A dataframe with a boolean column indicating whether each value
#'   exceeds the threshold.
#'
#' @export

is_above <- function(x, threshold) {
    n_day <- paste0("day_abv_", threshold)
    x |>
        dplyr::transmute({{n_day}} := value > threshold)
}
