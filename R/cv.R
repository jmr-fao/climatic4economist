#' Compute Coefficient of Variation (CV)
#'
#' This function calculates the coefficient of variation (CV), which is a
#' standardized measure of dispersion, defined as the ratio of the standard
#' deviation to the mean.
#'
#' @param x A numeric vector.
#' @param na_rm A logical value indicating whether to remove `NA` values before
#'   computation. Default is `TRUE`.
#'
#' @return A numeric value representing the coefficient of variation. If `x`
#'   contains only `NA` values or has a mean of zero, the function returns
#'   `NA_real_`.
#'
#' @export
#'
#' @examples
#' x <- c(10, 15, 20, 25, 30)
#' cv(x)  # Compute CV for a numeric vector
#'
#' x_with_na <- c(10, 15, NA, 25, 30)
#' cv(x_with_na)  # Removes NA by default
#' cv(x_with_na, na_rm = FALSE)  # Returns NA if missing values exist
#'
#' x_zero_mean <- c(-5, 5, -5, 5)
#' cv(x_zero_mean)  # Returns NA since mean is zero

cv <- function(x, na_rm = TRUE) {
    avg <- mean(x, na.rm = na_rm)
    if (is.nan(avg)) return(NA_real_)
    if (avg == 0) return(NA_real_)  # Avoid division by zero
    sd(x, na.rm = na_rm) / mean(x, na.rm = na_rm)
}
