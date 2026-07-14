#' Compute the mode of a vector
#'
#' Returns the most frequent value in a vector, optionally handling ties in
#' different ways. Missing values are ignored.
#'
#' @param x A vector.
#' @param ties Character string specifying how ties should be handled.
#'   One of:
#'   \describe{
#'     \item{"first"}{Return the first mode encountered (default).}
#'     \item{"all"}{Return all modes.}
#'     \item{"NA"}{Return a missing value if multiple modes exist.}
#'     \item{"error"}{Throw an error if the mode is not unique.}
#'   }
#'
#' @return
#' Depending on `ties`:
#' \itemize{
#'   \item `"first"` returns a single value of the same type as `x`.
#'   \item `"all"` returns a vector containing all modal values.
#'   \item `"NA"` returns a missing value of the same type as `x` when ties
#'   occur.
#'   \item `"error"` returns a single value if the mode is unique, otherwise
#'   throws an error.
#' }
#'
#' If `x` contains only missing values, a missing value of the same type as
#' `x` is returned.
#'
#' @examples
#' mode(c(1, 2, 2, 3))
#'
#' mode(c(1, 1, 2, 2), ties = "all")
#'
#' mode(c("a", "b", "a"))
#'
#' mode(c(NA, 1, 1, 2))
#'
#' @export
mode <- function(x, ties = c("first", "all", "NA", "error")) {
    ties <- match.arg(ties)

    x <- x[!is.na(x)]
    if (!length(x))
        return(x[NA_integer_])

    values <- unique(x)
    counts <- tabulate(match(x, values))
    modes <- values[counts == max(counts)]

    switch(
        ties,
        first = modes[[1L]],
        all = modes,
        NA = x[NA_integer_],
        error = {
            if (length(modes) > 1L)
                stop("Multiple modes found.", call. = FALSE)
            modes[[1L]]
        }
    )
}
