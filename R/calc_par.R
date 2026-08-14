#' Calculate Aggregated Parameters with Optional Temporal Aggregation
#'
#' Computes summary statistics for spatial units from time-indexed columns.
#' The function reshapes the input data to long format and applies user-defined
#' summary functions to the values associated with each spatial identifier.
#'
#' When `agg_period` is specified, the function performs a two-step aggregation:
#' values are first aggregated within each temporal period (e.g. year or month)
#' using the functions provided in `pars`, and the resulting statistics are then
#' averaged across periods. If `agg_period = NULL`, parameters are computed
#' directly across all available observations.
#'
#' @param df A data frame containing spatial identifier columns and numeric
#'   columns representing time-indexed observations. Time columns are expected
#'   to begin with a four-digit year (e.g. `"200101"`, `"2001-01-01"`).
#' @param pars A function or a named list of functions applied to the values.
#'   For example, `mean` or `list(avg = mean, total = sum)`. Function names
#'   are used to construct output column names.
#' @param prefix Optional character string added as a prefix to the resulting
#'   parameter columns. Defaults to `NULL`.
#' @param suffix Optional character string added as a suffix to the resulting
#'   parameter columns. Defaults to `NULL`.
#' @param agg_period Optional character string specifying an intermediate
#'   temporal aggregation level. Supported values are `"year"` and `"month"`.
#'   If provided, aggregation is performed within each period before averaging
#'   across periods. Defaults to `NULL`.
#' @param min_coverage Number between 0 and 1. The share of a month's days that
#'   must be present for that month to enter the average. Used only when
#'   `agg_period = "month"`. Defaults to `0.8`, so a 31-day month needs at
#'   least 25 observations. Set to `0` to keep every period.
#'
#' @section Monthly coverage:
#' With `agg_period = "month"` the values are first summarised within each
#' month and those monthly statistics are then averaged. A month represented by
#' only a handful of days would otherwise enter that average as though it were
#' a whole month, dragging the result toward the partial period. `min_coverage`
#' guards against this by comparing the observations present in each window
#' against the number of days the window actually spans.
#'
#' If no month reaches the threshold the function stops rather than returning
#' an empty result. The most common cause is passing data that is already
#' monthly: each period then holds a single observation, so there is nothing to
#' aggregate within a month. Use `agg_period = NULL` to summarise across all
#' observations, or `agg_period = "year"` to average monthly values by year.
#'
#' @return
#' A tibble with one row per spatial unit and columns containing the
#' aggregated statistics defined in `pars`. Output column names are taken from
#' the names of `pars`, optionally wrapped by `prefix` and `suffix` as
#' `"<prefix>_<name>_<suffix>"`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 2),
#'   `200101` = c(10, 20),
#'   `200102` = c(15, 25),
#'   `200201` = c(5, 15),
#'   `200202` = c(10, 20)
#' )
#'
#' # Mean across all observations
#' calc_par(df, pars = list(avg = mean))
#'
#' # Average of yearly totals
#' calc_par(
#'   df,
#'   pars = list(total = sum),
#'   agg_period = "year"
#' )

calc_par <- function(df, pars, prefix = NULL, suffix = NULL, agg_period = NULL,
                     min_coverage = 0.8) {

    stopifnot(
        "`min_coverage` must be a single number between 0 and 1" =
            is.numeric(min_coverage) && length(min_coverage) == 1 &&
            !is.na(min_coverage) && min_coverage >= 0 && min_coverage <= 1
    )

    id_vars <- c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")

    # Pivot to long format
    df_long <- df |>
        dplyr::select(dplyr::any_of(id_vars), dplyr::matches("[0-9]{4}")) |>
        tidyr::pivot_longer(cols = dplyr::matches("[0-9]{4}"),
                            names_to = "time_label",
                            values_to = "value")

    if (is.null(agg_period)) {
        # Process without intermediary grouping
        out <- df_long |>
            dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
            dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
                             .groups = "drop")
    } else {
        # Dispatch to specific logic
        out <- switch(agg_period,
                      "year"  = aggregate_by_year(df_long, id_vars, pars),
                      "month" = aggregate_by_month(df_long, id_vars, pars,
                                                   min_coverage = min_coverage),
                      stop("Unsupported `agg_period`. Use 'year', 'month'.")
        )
    }

    # rename with prefix
    if (!is.null(prefix)) {
        out <- out |>
            dplyr::rename_with(.fn = \(x) paste0(prefix, "_", x),
                               .cols = -dplyr::any_of(id_vars))
    }
    # rename with suffix
    if (!is.null(suffix)) {
        out <- out |>
            dplyr::rename_with(.fn = \(x) paste0(x, "_", suffix),
                               .cols = -dplyr::any_of(id_vars))
    }

    return(out)
}

aggregate_by_year <- function(df_long, id_vars, pars) {
    # find last date
    last_date <- max(df_long$time_label) |>
        to_date() |>
        lubridate::as_date()

    is_last_day_month <- last_date == lubridate::ceiling_date(last_date, "month") - 1
    is_december <- lubridate::month(last_date) == 12
    if (!is_last_day_month | !is_december) {

        msg <- paste0(
            "The last_date (", last_date, ") is not the last day of a month ",
            "and/or not in December.\n",
            "This date is used as the reference anchor to calculate time intervals ",
            "to avoid bias in the summary aggregation by year."
        )

        warning(msg, call. = FALSE)
    }

    df_long |>
        dplyr::mutate(
            time_label = to_date(time_label),
            time_group = find_lag(time_label, last_date, unit = "year")
            ) |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
        dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
                         .groups = "drop") |>
        # final aggregation
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -time_group,
                                       .fns = \(x) mean(x, na.rm = TRUE)),
                         .groups = "drop")
}
aggregate_by_month <- function(df_long, id_vars, pars, min_coverage = 0.8) {
    # find last date
    last_date <- max(df_long$time_label)|>
        to_date() |>
        lubridate::as_date()

    is_last_day_month <- last_date == lubridate::ceiling_date(last_date, "month") - 1
    if (!is_last_day_month) {

        msg <- paste0(
            "The last_date (", last_date, ") is not the last day of a month.\n",
            "This date is used as the reference anchor to calculate time intervals ",
            "to avoid bias in the summary aggregation by month."
        )

        warning(msg, call. = FALSE)
    }

    # intermediate aggregation, one row per unit and monthly window
    binned <- df_long |>
        dplyr::mutate(time_label = to_date(time_label),
                      time_group = find_lag(time_label, last_date, unit = "month")) |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
        dplyr::summarise(n_days = dplyr::n(),
                         dplyr::across(value, pars, .names = "{fn}"),
                         .groups = "drop")

    # Each window runs from one monthly step before the anchor to the next, so
    # its length follows the calendar: 31 days, then 30, then 31. Judging
    # completeness against that length keeps whole months and drops slivers,
    # rather than dropping only windows holding a single observation.
    binned <- binned |>
        dplyr::mutate(
            expected_days = as.numeric(
                clock::add_months(last_date, time_group, invalid = "previous") -
                clock::add_months(last_date, time_group - 1L, invalid = "previous")),
            is_complete = n_days >= min_coverage * expected_days)

    kept <- dplyr::filter(binned, is_complete)

    if (nrow(kept) == 0) {
        stop(incomplete_period_message(binned, min_coverage), call. = FALSE)
    }

    # final aggregation
    kept |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -c(time_group, n_days,
                                                  expected_days, is_complete),
                                       .fns = \(x) mean(x, na.rm = TRUE)),
                         .groups = "drop")
}

#' Explain why no monthly window survived the coverage filter
#' @noRd
incomplete_period_message <- function(binned, min_coverage) {
    if (all(binned$n_days == 1)) {
        return(paste0(
            "No monthly period has enough observations to aggregate.\n",
            "Every period holds exactly one observation, so the data is ",
            "already at monthly resolution and there is nothing to aggregate ",
            "within a month. Use `agg_period = NULL` to summarise across all ",
            "observations, or `agg_period = \"year\"` to average monthly ",
            "values within each year."
        ))
    }

    best <- max(binned$n_days / binned$expected_days)
    paste0(
        "No monthly period reaches the required coverage of ",
        format(min_coverage), ".\n",
        "The most complete period covers ", format(round(best, 2)),
        " of its days. Lower `min_coverage` to accept partial months, or ",
        "check the input for gaps."
    )
}
