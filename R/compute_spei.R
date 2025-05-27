#' Compute the SPEI
#'
#' Compute the Standardized Precipitation-Evapotranspiration Index (SPEI) for
#' unique locations. Under the hood the function performs parallel computation
#' to speed up the computation with [furrr::future_map].
#'
#' @param df A data frame. It must contains an `ID` variable and the
#'   precipitation observation in wide format, i.e. each column is a different
#'   observation in time.
#' @param time_scale an integer, representing the time scale at which the SPEI
#'  will be computed.
#' @returns A \link[tibble]{tbl_df}, It must contains an `ID` variable and the
#'   SPEI observation in wide format, i.e. each column is a different
#'   observation in time and each row is a unique location.
#' @export
#' @import dplyr tidyr furrr
#' @seealso [SPEI::spei]
#' @examples
#' compute_spei(coord, time_scale = 1)
compute_spei <- function(df, time_scale) {
    df |>
        dplyr::select(ID, dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::distinct(ID, .keep_all = TRUE) |>
        tidyr::pivot_longer(cols = -ID) |>
        tidyr::nest(data = c(name, value)) |>
        dplyr::mutate(spei = furrr::future_map(data,
                                               spei_wide,
                                               scale = time_scale)) |>
        dplyr::select(-data) |>
        tidyr::unnest(spei)
}


#' Compute the SPEI
#'
#' Compute the Standardized Precipitation-Evapotranspiration Index (SPEI) in
#' wide format.
#'
#' @param df_list A data frame. It must contains an `value` variable and a
#'   variable `name` with the date of observations.
#' @param scale an integer, representing the time scale at which the SPEI
#'   will be computed.
#' @returns A \link[tibble]{tbl_df}, with the SPEI values in wide format, i.e.
#'   each column is an observation in time.
#'
#' @seealso [SPEI::spei]
#'
#' @examples
#' spei_wide(data, scale = 1)
#'
spei_wide <- function(df_list, scale = 1) {
    SPEI::spei(df_list$value, scale = scale, verbose = FALSE, na.rm = TRUE) |>
        fitted() |>
        t() |>
        as.data.frame() |>
        setNames(df_list$name)
}
