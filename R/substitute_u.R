#' Replace Values Above a Threshold with NA
#'
#' This function replaces values in a numeric vector that are above a given threshold with `NA_real_`.
#'
#' @param x A numeric vector.
#' @param threshold A numeric value specifying the upper bound. Values above this threshold will be replaced with `NA_real_`.
#'
#' @return A numeric vector with values above the threshold replaced by `NA_real_`.
#'
#' @export
#'
#' @examples
#' vec <- c(2, 5, 8, 1, 10)
#' substitute_u(vec, threshold = 5)
#' # Returns: c(2, 5, NA, 1, NA)

substitute_u <- function(x, threshold) {
    if (!is.null(threshold)) {
        x[x > threshold] <- NA_real_
    }
    x
}
