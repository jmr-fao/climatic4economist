#' Calculate sample skewness
#'
#' Calculates the third standardized central moment of a numeric vector.
#' Missing values are removed before calculation.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value giving the skewness of \code{x}.
#'
#' @details
#' Skewness is calculated as the third central moment divided by the
#' cube of the standard deviation:
#' \deqn{
#' \frac{\mathrm{mean}[(x - \bar{x})^3]}{\mathrm{sd}(x)^3}
#' }
#' Missing values (\code{NA}) are removed before calculation.
#'
#' @examples
#' x <- c(1, 2, 2, 3, 10)
#' skewness(x)
#'
#' @export
skewness <- function(x) {
    x <- x[!is.na(x)]
    mean((x - mean(x))^3) / stats::sd(x)^3
}
