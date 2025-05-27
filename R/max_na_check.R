#' Compute the maximum of a numeric vector while handling missing values
#'
#' This function calculates the maximum value of a numeric vector while handling
#' `NA` values. If all values in the vector are `NA`, it returns a specified
#' replacement value.
#'
#' @param x A numeric vector.
#' @param replace A numeric value to return if all elements in `x` are `NA`.
#'   Defaults to `0`.
#'
#' @return A numeric value representing the maximum of `x`, ignoring `NA`
#'   values. If all values in `x` are `NA`, it returns `replace`.
#'
#' @export
#'
#' @examples
#' # Example with no missing values
#' max_na_check(c(1, 2, 3, 4, 5))  # Returns 5
#'
#' # Example with missing values
#' max_na_check(c(1, 2, NA, 4, 5))  # Returns 5
#'
#' # Example where all values are NA
#' max_na_check(c(NA, NA, NA))  # Returns 0
#'
#' # Example with a custom replacement value
#' max_na_check(c(NA, NA, NA), replace = -1)  # Returns -1

max_na_check <- function(x, replace = 0) {
    if (all(is.na(x))) { return(as.numeric(replace)) }
    return(as.numeric(max(x, na.rm = TRUE)))
}
