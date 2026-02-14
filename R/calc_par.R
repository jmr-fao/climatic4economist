#' Calculate Aggregated Parameters with Optional Temporal Intermediary
#'
#' Computes summary statistics for spatial units from time-indexed columns.
#' The function reshapes the input data to long format and applies user-defined
#' summary functions to the values associated with each spatial identifier.
#'
#' If an aggregation period is provided through `agg_period`, the function
#' first aggregates observations within each time period (e.g., yearly totals
#' or monthly means) and then computes the average of those intermediate
#' statistics across periods. If `agg_period = NULL`, parameters are computed
#' directly using all available observations.
#'
#' @param df A data frame containing spatial identifier columns and numeric
#'   columns representing time-indexed observations. Time columns are expected
#'   to follow a numeric or character format beginning with a four-digit year
#'   (e.g., `"200101"`, `"2001-01-01"`).
#' @param pars A function or a named list of functions applied to the values.
#'   For example, `mean` or `list(avg = mean, total = sum)`. Function names
#'   are used to construct output column names.
#' @param prefix Optional character string added as a prefix to the resulting
#'   parameter columns. Defaults to `NULL`.
#' @param agg_period Optional character string specifying an intermediate
#'   temporal aggregation level. Allowed values are `"year"`, `"month"`,
#'   or `"day"`. When provided, the function:
#'   \enumerate{
#'     \item Aggregates observations within each period using `pars`;
#'     \item Computes the mean of those aggregated values across periods.
#'   }
#'   Defaults to `NULL`, in which case parameters are computed directly
#'   across all observations.
#'
#' @details
#' The function assumes that spatial identifiers are stored in one or more of
#' the following columns when present:
#' `"ID"`, `"ID_adm_div"`, `"x_cell"`, `"y_cell"`, and
#' `"coverage_fraction"`. Only existing columns are used.
#'
#' Time labels are internally converted to dates using [climatic4economist::to_date]
#'
#' @return
#' A tibble with one row per spatial unit and columns containing the
#' aggregated statistics defined in `pars`.
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

calc_par <- function(df, pars, prefix = NULL, agg_period = NULL) {

    id_vars <- c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")

    # Pivot to long format
    df_long <- df |>
        dplyr::select(dplyr::any_of(id_vars), dplyr::matches("[0-9]{4}")) |>
        tidyr::pivot_longer(
            cols = dplyr::matches("[0-9]{4}"),
            names_to = "time_label",
            values_to = "value"
        )

    # Process with intermediary grouping
    if (!is.null(time_unit)) {
        out <- df_long |>
            dplyr::mutate(
                # Standardize date and extract group
                time_label = climatic4economist::to_date(time_label),
                time_group = dplyr::case_when(
                    time_unit == "year"  ~ substr(time_label, 1, 4),
                    time_unit == "month" ~ substr(time_label, 1, 7),
                    time_unit == "day" ~ substr(time_label, 1, 10),
                    TRUE ~ time_label)) |>
            # intermediate aggregation
            dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars)), time_group) |>
            dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
                             .groups = "drop") |>
            # final aggregation
            dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
            dplyr::summarise(dplyr::across(.cols = -time_group, .fns = mean),
                             .groups = "drop")

    } else {
        # Process without intermediary grouping
        out <- df_long |>
            dplyr::group_by(dplyr::pick(dplyr::any_of(id_vars))) |>
            dplyr::summarise(dplyr::across(value, pars, .names = "{fn}"),
                             .groups = "drop")
    }

    # rename with prefix
    if (!is.null(prefix)) {
        out <- out |>
            dplyr::rename_with(.fn = \(x) paste0(prefix, "_", x),
                               .cols = -dplyr::any_of(id_vars))
    }

    return(out)
}
