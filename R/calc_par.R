#' Calculate Aggregated Parameters with Optional Temporal Intermediary
#'
#' Computes summary statistics for spatial units. If a `time_unit` is provided,
#' it first aggregates data to that level (e.g., yearly sums) before calculating
#' the final average across those periods.
#'
#' @param df A data frame with spatial ID columns and numeric columns (expected year-month-day).
#' @param pars A function or a named list of functions (e.g., \code{list(avg = mean)}).
#' @param prefix Optional string to prefix the resulting parameter columns.
#' @param time_unit Optional string: \code{"year"}, \code{"month"}, or \code{day}'
#'  If \code{NULL}, aggregates all values directly.
#'
#' @return A \code{tibble} with aggregated statistics for each ID.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 2),
#'   `200101` = c(10, 20), `200102` = c(15, 25),
#'   `200201` = c(5, 15),  `200202` = c(10, 20)
#' )
#' # Average of yearly sums
#' calc_par(df, pars = list(total = sum), time_unit = "year")
calc_par <- function(df, pars, prefix = NULL, time_unit = NULL) {

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
        out |>
            dplyr::rename_with(.fn = \(x) paste0(prefix, "_", x),
                               .cols = -dplyr::any_of(id_vars))
    }

    return(out)
}
