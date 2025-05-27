#' Compute sample percentiles of a numeric vector while handling missing
#' values
#'
#' This function calculates sample percentiles corresponding to the given
#' probabilities. If all values in the vector are `NA`, it returns a
#' specified replacement value.
#'
#' @param x A numeric vector.
#' @param p numeric vector of probabilities with values in [0, 1].
#' @param replace A numeric value to return if all elements in `x` are `NA`.
#'   Defaults to `0`.
#'
#' @return A numeric value representing the standard deviation of `x`, ignoring
#'   `NA` values. If all values in `x` are `NA`, it returns `replace`.
#'
#' @seealso [quantile()]
#'
#' @export

quantile_na_check <- function(x, p, replace = 0) {
    if (all(is.na(x))) {
        return(
            setNames(rep(replace, length(p)), paste0(p*100, "%")) )
    }
    return(quantile(x, p, na.rm = TRUE))
}
