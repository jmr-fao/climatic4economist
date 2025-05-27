#' Replace Values Below a Threshold with NA
#'
#' This function replaces values in a numeric vector that are below a given
#' threshold with `NA_real_`.
#'
#' @param x A numeric vector.
#' @param threshold A numeric value specifying the lower bound. Values below
#'   this threshold will be replaced with `NA_real_`.
#'
#' @return A numeric vector with values below the threshold replaced by
#'   `NA_real_`.
#'
#' @export
#'
#' @examples
#' vec <- c(2, 5, 8, 1, 10)
#' substitute_l(vec, threshold = 5)
#' # Returns: c(NA, 5, 8, NA, 10)

substitute_l <- function(x, threshold) {
    if (!is.null(threshold)) {
        x[x < threshold] <- NA_real_
    }
    x
}
