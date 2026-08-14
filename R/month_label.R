#' Parse date labels into a Date vector
#'
#' Converts the date labels produced by reshaping a wide weather table into a
#' proper `Date` vector. [data.table::melt()] returns the former column names as
#' a **factor**, whose levels follow the order the columns appeared in rather
#' than chronological order. Sorting or computing spells on that factor silently
#' orders rows by column position, so the labels must be parsed before any
#' ordering is done.
#'
#' Parsing uses [lubridate::ymd()] with `truncated = 2`, which accepts full
#' dates (`"2022-01-15"`), year-month labels (`"2022-01"`, common for monthly
#' series) and unpadded months (`"2022-1-15"`). [clock::date_parse()] cannot
#' read the truncated form and returns `NA` for it.
#'
#' @param x A `Date` or `POSIXt` vector, or a character/factor vector of date
#'   labels in year-month-day order.
#'
#' @return A `Date` vector the same length as `x`, `NA` where a label could not
#'   be parsed.
#'
#' @noRd
parse_date_label <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(lubridate::as_date(x))

    # Long-format data repeats each label once per unit, so parse the distinct
    # labels only and expand the result back over the input.
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
    parsed[idx]
}

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
#'   dates in year-month-day order.
#'
#' @return A character vector of two-digit month labels, `NA_character_` where
#'   the input could not be parsed.
#'
#' @noRd
month_label <- function(x) {
    pad_month(lubridate::month(parse_date_label(x)))
}

#' Format month numbers as two-digit labels, preserving NA
#' @noRd
pad_month <- function(m) {
    out <- formatC(m, width = 2, flag = "0")
    out[is.na(m)] <- NA_character_
    out
}
