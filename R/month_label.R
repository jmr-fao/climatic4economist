#' Extract the calendar month as a zero-padded label
#'
#' Returns the month of a date-like vector as a two-character string
#' (`"01"` ... `"12"`). The label is used as a grouping and joining key between
#' the monthly percentile thresholds and the daily observations, so it must be
#' padded consistently on both sides of the join.
#'
#' Parsing is delegated to \pkg{lubridate} rather than done by string position.
#' A positional approach such as `substr(x, 6, 7)` silently returns the wrong
#' value whenever the month is not zero padded (`"2024-1-15"` yields `"1-"`) or
#' the string is truncated to a year and month (`"2024-1"` yields `"1"`).
#'
#' @param x A `Date` or `POSIXt` vector, or a character/factor vector holding
#'   dates in year-month-day order. Values truncated to year-month, or to year
#'   alone, are accepted.
#'
#' @return A character vector of two-digit month labels, `NA_character_` where
#'   the input could not be parsed.
#'
#' @noRd
month_label <- function(x) {
    if (inherits(x, c("Date", "POSIXt"))) {
        return(pad_month(lubridate::month(x)))
    }

    # Reshaping to long format repeats each date label once per unit, so parse
    # the distinct labels only and expand the result back over the input.
    if (is.factor(x)) {
        labels <- levels(x)
        idx <- as.integer(x)
    } else {
        x <- as.character(x)
        labels <- unique(x)
        idx <- match(x, labels)
    }

    parsed <- suppressWarnings(
        lubridate::ymd(labels, truncated = 2, quiet = TRUE)
    )
    pad_month(lubridate::month(parsed))[idx]
}

#' Format month numbers as two-digit labels, preserving NA
#' @noRd
pad_month <- function(m) {
    out <- formatC(m, width = 2, flag = "0")
    out[is.na(m)] <- NA_character_
    out
}
