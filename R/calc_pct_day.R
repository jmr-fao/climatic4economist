#' Compute daily percentile for specific location and month
#'
#' This function calculates the specified percentiles of daily values for each
#' month, optionally filtering values based on lower and upper
#' thresholds. Optionally it calculates the specified percentiles of daily values
#' for the entire year.
#'
#' @param df A dataframe containing an ID column and date-based columns.
#' @param iteracation optional character to be print before computation. Usually,
#'  it is the name of the object on which the function is applied. This is useful
#'  when the function is used inside an apply family function to keep track of the
#'  iterations.
#' @param p A numeric vector specifying the percentiles to compute (values
#'   between 0 and 1).
#' @param l_thresh (Optional) A numeric value specifying the lower threshold.
#'   Values below this will be excluded.
#' @param u_thresh (Optional) A numeric value specifying the upper threshold.
#'   Values above this will be excluded.
#' @param yearly (optional) Logical, if TRUE calculate also yearly daily values.
#'
#'
#' @return A dataframe with computed percentile-based day values for each month,
#'   with column names prefixed by "day_".
#'
#' @export
#' @import data.table
#'
#' @examples
#' calc_pct_day(df, p = c(0.25, 0.5, 0.75), l_thresh = 60, u_thresh = 0.1)

calc_pct_day <- function(df,
                         iteracation = NULL,
                         p,
                         l_thresh = NULL,
                         u_thresh = NULL,
                         yearly = FALSE) {

    stopifnot("Either `l_thresh` or `u_thresh` is allowed, not both" =
                  !(!is.null(l_thresh) & !is.null(u_thresh)))
    stopifnot("`l_thresh` must have length 1" = length(l_thresh) <= 1)
    stopifnot("`u_thresh` must have length 1" = length(u_thresh) <= 1)

    if (!is.null(iteracation)) cat("calculating percentile:", iteracation, "\n")

    df_long <- df |>
        dplyr::select(ID, dplyr::matches("[0-9]{4}")) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        dplyr::rename_with(to_date) |>
        data.table::as.data.table() |>
        data.table::melt(id.vars = "ID",
                         variable.name = "date",
                         value.name = "value")

    df_long[, month := substr(date, 6, 7), ]

    if (!is.null(l_thresh)) df_long[value < l_thresh, value := NA_real_]
    if (!is.null(u_thresh)) df_long[value > u_thresh, value := NA_real_]

    pct_mnth <- df_long[, quantile_df(value, p, replace = c(l_thresh, u_thresh)),
            by = .(ID, month)] |>
        dplyr::rename_with(.cols = -c(ID, month),
                           .fn = ~paste0("day_", .x))
    if (yearly) {
        pct_year <- df_long[, quantile_df(value, p, replace = c(l_thresh, u_thresh)),
                            by = .(ID)] |>
            dplyr::rename_with(.cols = -c(ID),
                               .fn = ~paste0("yr_", .x))

        data.table::merge.data.table(pct_mnth,
                                     pct_year,
                                     by = "ID",
                                     allow.cartesian = TRUE)
    } else {
        pct_mnth
    }
}
