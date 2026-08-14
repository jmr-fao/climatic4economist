#' Summarise daily extreme indicators over the periods before each interview
#'
#' Aggregates daily extreme-event indicators into one figure per unit and lag,
#' counting only the days that fall in the reference periods preceding each
#' unit's interview date.
#'
#' This is the fast counterpart to [extr_day_index()]. Rather than merging the
#' indicator data onto the survey first and filtering afterwards, it takes the
#' two tables separately and never builds the intermediate. See Details.
#'
#' @param survey A data frame with one row per unit (typically a household),
#'   containing the `by`, `id` and `interview` columns.
#' @param indicators A data frame of daily extreme indicators with a `date`
#'   column and the `by` column, as returned by [find_extr_abs_day()] or
#'   [find_extr_rel_day()].
#' @param by <[`data-masking`][rlang::args_data_masking]> The column linking
#'   `survey` to `indicators`, typically the location `ID` created by
#'   [prepare_coord()].
#' @param id <[`data-masking`][rlang::args_data_masking]> The column identifying
#'   each unit of the result, typically the household identifier.
#' @param interview <[`data-masking`][rlang::args_data_masking]> The column
#'   holding the interview dates, or a single date applied to every row.
#' @param interval A string specifying the reference period, e.g. `"1 year"`,
#'   `"3 months"`, `"30 days"`.
#' @param n_lags Integer. The highest lag, in units of `interval`, to compute.
#'   `0` covers only the period immediately before the interview. Default `0`.
#' @param extra_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional columns
#'   of `survey` carried through unchanged.
#' @param iteration Optional character printed before computation. Useful when
#'   the function is called inside an apply family function to keep track of the
#'   iterations.
#'
#' @details
#' The value of an indicator depends only on the location, the interview date
#' and the lag — never on which unit was interviewed. Units sharing a location
#' and an interview date therefore have identical results. The function
#' exploits this: it builds the reference windows from the *distinct*
#' `by`/`interview` pairs, aggregates once per window using an inequality join,
#' and only then broadcasts the result back to the individual units.
#'
#' Because the join carries the window bounds as its matching condition, days
#' outside the reference periods are never materialised. [extr_day_index()]
#' instead receives an already-merged table holding every unit crossed with
#' every day, and discards most of it.
#'
#' Lag `L` covers the dates lying between `L + 1` and `L` periods before the
#' interview. The interview date itself is excluded, so lag 0 ends the day
#' before it. Units whose interview date is missing are dropped, as are units
#' with no indicator data inside any window.
#'
#' The window bounds are shifted with `invalid = "previous"`, matching
#' [filter_by_interview()]: a boundary landing on a date that does not exist
#' falls back to the last valid day of that month. The windows tile the
#' reference span without gaps or overlaps.
#'
#' One day can be attributed differently here than by [extr_day_index()], and
#' only when the interview falls on 28 February. `extr_day_index()` asks how
#' many whole years separate a day from the interview; for 29 February 2020 and
#' an interview on 28 February 2023 the third anniversary rolls back onto the
#' interview date itself, so three whole years fit and the day is assigned
#' lag 3. This function instead cuts windows at 28 February of each year, so
#' the same day falls one day inside the lag 2 window. Both readings tile the
#' span completely; they disagree only on which lag owns the leap day. Every
#' other interview date gives identical results.
#'
#' @return A tibble with one row per `id` and `lag`, containing the summed
#'   day counts (`day_*_sum`) and summed unit values (`*_abv_*_sum`,
#'   `*_blw_*_sum`).
#'
#' @seealso [extr_spell_by_interview()] for spell data, [extr_day_index()] for
#'   the merge-first equivalent.
#'
#' @export
#'
#' @examples
#' survey <- data.frame(
#'   hhid = 1:2,
#'   ID = c("1", "1"),
#'   interview = as.Date("2024-01-01")
#' )
#' indicators <- data.frame(
#'   ID = "1",
#'   date = as.Date(c("2023-06-01", "2023-07-01", "2022-06-01")),
#'   day_abv_90p = c(1, 0, 1)
#' )
#' extr_day_by_interview(survey, indicators, by = ID, id = hhid,
#'                       interview = interview, interval = "1 year", n_lags = 1)
extr_day_by_interview <- function(survey,
                                  indicators,
                                  by,
                                  id,
                                  interview,
                                  interval,
                                  n_lags = 0,
                                  extra_col = NULL,
                                  iteration = NULL) {

    check_by_interview_args(by, id, interview, interval)

    if (!is.null(iteration)) cat("Computing extreme day index:", iteration, "\n")

    totals <- aggregate_by_interview(
        survey     = survey,
        indicators = indicators,
        by         = {{by}},
        interview  = {{interview}},
        interval   = interval,
        n_lags     = n_lags,
        .fns       = function(data) {
            dplyr::summarise(
                data,
                dplyr::across(
                    .cols  = dplyr::matches("^day"),
                    .fns   = ~ sum_na_check(.x),
                    .names = "{.col}_sum"),
                dplyr::across(
                    .cols  = dplyr::matches("abv|blw") & !dplyr::matches("day"),
                    .fns   = ~ sum_na_check(.x),
                    .names = "{.col}_sum"),
                .by = c(".by_key", ".interview", "lag"))
        })

    broadcast_to_units(survey, totals, {{by}}, {{id}}, {{interview}},
                       {{extra_col}})
}

#' Summarise spell indicators over the periods before each interview
#'
#' Aggregates spell-length data into summary statistics per unit and lag,
#' counting only the spells ending in the reference periods preceding each
#' unit's interview date.
#'
#' This is the fast counterpart to [extr_spell_index()], and works exactly as
#' [extr_day_by_interview()] does — see its Details for how the reference
#' windows are built and why the intermediate merge is avoided.
#'
#' @inheritParams extr_day_by_interview
#' @param indicators A data frame of spell data with a `date` column, the `by`
#'   column and columns starting with `spell`, as returned by
#'   [find_spell()] or [find_extr_spell_rel()].
#'
#' @return A tibble with one row per `id` and `lag`, containing for each spell
#'   column its longest (`_max`), average (`_mean`) and total (`_sum`) length,
#'   and the number of spells (`_n`).
#'
#' @seealso [extr_day_by_interview()] for daily data, [extr_spell_index()] for
#'   the merge-first equivalent.
#'
#' @export
#'
#' @examples
#' survey <- data.frame(
#'   hhid = 1:2,
#'   ID = c("1", "1"),
#'   interview = as.Date("2024-01-01")
#' )
#' spells <- data.frame(
#'   ID = "1",
#'   date = as.Date(c("2023-06-01", "2023-07-01")),
#'   spell_abv_90p = c(3, 5)
#' )
#' extr_spell_by_interview(survey, spells, by = ID, id = hhid,
#'                         interview = interview, interval = "1 year")
extr_spell_by_interview <- function(survey,
                                    indicators,
                                    by,
                                    id,
                                    interview,
                                    interval,
                                    n_lags = 0,
                                    extra_col = NULL,
                                    iteration = NULL) {

    check_by_interview_args(by, id, interview, interval)

    if (!is.null(iteration)) cat("Computing extreme spell index:", iteration, "\n")

    totals <- aggregate_by_interview(
        survey     = survey,
        indicators = indicators,
        by         = {{by}},
        interview  = {{interview}},
        interval   = interval,
        n_lags     = n_lags,
        .fns       = function(data) {
            dplyr::summarise(
                data,
                dplyr::across(
                    .cols = dplyr::matches("^spell"),
                    .fns  = list(max  = ~ max_na_check(.x),
                                 mean = ~ mean_na_check(.x),
                                 sum  = ~ sum_na_check(.x),
                                 n    = ~ sum(!is.na(.x)))),
                .by = c(".by_key", ".interview", "lag"))
        })

    broadcast_to_units(survey, totals, {{by}}, {{id}}, {{interview}},
                       {{extra_col}})
}

#' Check the required arguments of the `*_by_interview()` functions
#'
#' The messages deliberately echo those of [extr_day_index()] so that the two
#' families fail the same way.
#'
#' @noRd
check_by_interview_args <- function(by, id, interview, interval) {
    if (missing(interview)) {
        stop("Error: provide a date of interview or the column name with the dates of interview",
             call. = FALSE)
    }
    if (missing(interval)) {
        stop("Error: provide a time interval over which the aggregation is computer. E.g. `1 year`",
             call. = FALSE)
    }
    if (missing(id)) {
        stop("Error: provide a column name specifying the unique identifier for each unit",
             call. = FALSE)
    }
    if (missing(by)) {
        stop("Error: provide a column name linking the survey to the indicators, e.g. `ID`",
             call. = FALSE)
    }
    invisible(TRUE)
}

#' Aggregate indicators inside each interview reference window
#'
#' Shared engine of [extr_day_by_interview()] and [extr_spell_by_interview()].
#' The key and interview columns are renamed to fixed internal names so the
#' inequality join can be written literally, then `.fns` supplies whichever
#' summary statistics the caller wants.
#'
#' @param .fns A function taking the joined data frame and returning the
#'   summarised one. It must group by `.by_key`, `.interview` and `lag`.
#'
#' @return A data frame with `.by_key`, `.interview`, `lag` and the summaries.
#'
#' @noRd
aggregate_by_interview <- function(survey, indicators, by, interview,
                                   interval, n_lags, .fns) {

    parsed <- parse_interval(interval)

    if (!"date" %in% names(indicators)) {
        stop("`indicators` must contain a `date` column.", call. = FALSE)
    }

    keys <- survey |>
        dplyr::transmute(.by_key    = {{by}},
                         .interview = parse_date_label({{interview}}))

    windows <- make_lag_windows(keys, .by_key, .interview,
                                n_period = parsed$n,
                                period   = parsed$unit,
                                n_lags   = n_lags)

    indicators |>
        dplyr::rename(.by_key = {{by}}) |>
        dplyr::mutate(date = parse_date_label(date)) |>
        dplyr::inner_join(
            windows,
            by = dplyr::join_by(.by_key, date > w_start, date <= w_end)) |>
        .fns()
}

#' Broadcast window-level results back to the individual units
#'
#' The aggregation is done once per location and interview date; this step hands
#' the result to every unit sharing that pair. An inner join is used so units
#' with a missing interview date, or with no indicator data in any window, are
#' dropped — matching [extr_day_index()].
#'
#' @noRd
broadcast_to_units <- function(survey, totals, by, id, interview, extra_col) {

    id_name <- rlang::as_name(rlang::enquo(id))

    survey |>
        dplyr::transmute({{id}},
                         .by_key    = {{by}},
                         .interview = parse_date_label({{interview}}),
                         {{extra_col}}) |>
        # each key/interview pair fans out to every unit sharing it, and to
        # every lag: the many-to-many is the point of the broadcast
        dplyr::inner_join(totals,
                          by = dplyr::join_by(.by_key, .interview),
                          relationship = "many-to-many") |>
        dplyr::relocate(lag, .after = dplyr::all_of(id_name)) |>
        dplyr::select(-".by_key", -".interview") |>
        tibble::as_tibble()
}
