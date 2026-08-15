#' Regular expression matching the date columns of a wide weather table
#'
#' Wide weather tables carry one column per observation date, named for that
#' date. The functions that reshape them have to tell those columns apart from
#' everything else the table carries, which in practice is the whole survey:
#' [prepare_coord()] adds an identifier to the survey and the result is handed
#' straight to the reshapers, so household variables sit alongside the dates.
#'
#' A date column starts with its year. Anchoring on that is what separates the
#' two: no survey variable begins with four digits, while `income_2019` and
#' `hh_size_2020` both contain them. An unanchored pattern pulls those in, and
#' because [to_date()] rewrites `_` to `-`, `lubridate::ymd(truncated = 2)`
#' then reads `income-2019` as 1 January 2019 — the household's income enters
#' the series as a weather observation, with no error and no `NA`.
#'
#' The optional `X` is not decoration. R cannot name a column with a leading
#' digit, so `make.names()` and `haven::read_dta()` prefix one: a table read
#' from Stata arrives with `X2022.01.01`, which is the very form [to_date()]
#' exists to undo. Dropping `X?` would make the pattern miss the normal case.
#'
#' Nothing after the year is required, so year-only (`"2022"`), year-month
#' (`"2022-01"`, `"200101"`) and full dates all match.
#'
#' @return A single string, for use as `dplyr::matches(date_pattern())`.
#'
#' @noRd
date_pattern <- function() "^X?[0-9]{4}"
