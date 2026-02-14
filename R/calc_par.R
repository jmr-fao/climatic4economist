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
#'
#' @return
#' A tibble with one row per spatial unit and columns containing the
#' aggregated statistics defined in `pars`. Output column names follow
#' the pattern:
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

calc_par <- function(df, pars, prefix = NULL, suffix = NULL, agg_period = NULL) {

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
                      "month" = aggregate_by_month(df_long, id_vars, pars),
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
        lubridate::as_date()

    df_long |>
        dplyr::mutate(
            time_label = to_date(time_label),
            date_clean = lubridate::as_date(time_label),
            # Calculate months from the end
            total_months = lubridate::interval(date_clean, last_date) %/% months(1),

            # Divide into 12-month blocks (0, 1, 2...)
            # We use pmin(..., 1) to ensure anything older than 23 months
            # (like that 1st day) stays in the 'oldest' year group (index 1).
            time_group = pmin(total_months %/% 12, 1)) |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
        dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
                         .groups = "drop") |>
        # final aggregation
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -time_group, .fns = mean),
                         .groups = "drop")
}
aggregate_by_month <- function(df_long, id_vars, pars) {
    # find last date
    last_date <- max(df_long$time_label)
    last_day <- lubridate::day(last_date)

    # compute parameter
    df_long |>
        dplyr::mutate(time_label = to_date(time_label),
                      time_label = lubridate::as_date(time_label),
                      shifted_date = time_label - lubridate::days(start_day - 1),
                      time_group = format(shifted_date, "%Y-%m"))|>
        # intermediate aggregation
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
        dplyr::summarise(n_days = dplyr::n(),
                         dplyr::across(value, pars, .names = "{fn}"),
                         .groups = "drop") |>
        dplyr::filter(n_days > 1) |>
        # final aggregation
        dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
        dplyr::summarise(dplyr::across(.cols = -c(time_group, n_days),
                                       .fns = mean),
                         .groups = "drop")
}

# calc_par <- function(df, pars, prefix = NULL, suffix = NULL, agg_period = NULL) {
#
#     id_vars <- c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")
#
#     # Pivot to long format
#     df_long <- df |>
#         dplyr::select(dplyr::any_of(id_vars), dplyr::matches("[0-9]{4}")) |>
#         tidyr::pivot_longer(
#             cols = dplyr::matches("[0-9]{4}"),
#             names_to = "time_label",
#             values_to = "value"
#         )
#
#     # Process with intermediary grouping
#     if (!is.null(agg_period)) {
#         out <- df_long |>
#             dplyr::mutate(
#                 # Standardize date and extract group
#                 time_label = climatic4economist::to_date(time_label),
#                 time_group = dplyr::case_when(
#                     agg_period == "year"  ~ substr(time_label, 1, 4),
#                     agg_period == "month" ~ substr(time_label, 1, 7),
#                     agg_period == "day" ~ substr(time_label, 1, 10),
#                     TRUE ~ time_label)) |>
#             # intermediate aggregation
#             dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
#             dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
#                              .groups = "drop") |>
#             # final aggregation
#             dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
#             dplyr::summarise(dplyr::across(.cols = -time_group, .fns = mean),
#                              .groups = "drop")
#
#     } else {
#         # Process without intermediary grouping
#         out <- df_long |>
#             dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
#             dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
#                              .groups = "drop")
#     }
#
#     # rename with prefix
#     if (!is.null(prefix)) {
#         out <- out |>
#             dplyr::rename_with(.fn = \(x) paste0(prefix, "_", x),
#                                .cols = -dplyr::any_of(id_vars))
#     }
#
#     # rename with suffix
#     if (!is.null(suffix)) {
#         out <- out |>
#             dplyr::rename_with(.fn = \(x) paste0(x, "_", suffix),
#                                .cols = -dplyr::any_of(id_vars))
#     }
#
#     return(out)
# }
