#' Compute the Standardized Precipitation Index
#'
#' Compute the Standardized Precipitation Index (SPI) for unique locations.
#' Under the hood the function performs parallel computation to speed up the
#' computation with [furrr::future_map].
#'
#' @param df A data frame. It must contains an `ID` variable and the
#'   precipitation observation in wide format, i.e. each column is a different
#'   observation in time.
#' @param time_scale an integer, representing the time scale at which the SPEI /
#'   SPI will be computed.
#'
#' @returns A \link[tibble]{tbl_df}, It must contains an `ID` variable and the
#'   SPI observation in wide format, i.e. each column is a different
#'   observation in time and each row is a unique location.
#'
#' @seealso [SPEI::spi]
#'
#' @export
#'
#' @examples
#' compute_spi(coord, time_scale = 1)

compute_spi <- function(df, time_scale) {
    df |>
        dplyr::select(
            dplyr::any_of(c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")),
            dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::distinct(
            dplyr::pick(
                dplyr::any_of(c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction"))),
            .keep_all = TRUE) |>
        tidyr::pivot_longer(cols = dplyr::matches("[0-9]{4}.[0-9]{2}")) |>
        dplyr::group_by(dplyr::pick(
            dplyr::any_of(c("ID", "ID_adm_div", "x_cell", "y_cell", "coverage_fraction")))) |>
        dplyr::arrange(name, .by_group = TRUE) |>
        dplyr::ungroup() |>
        tidyr::nest(data = c(name, value)) |>
        dplyr::mutate(spi = furrr::future_map(data,
                                              spi_wide,
                                              scale = time_scale)) |>
        dplyr::select(-data) |>
        tidyr::unnest(spi)
}

#' Compute the SPI
#'
#' Compute the Standardized Precipitation Index (SPI) in wide format.
#'
#' @param df_list A data frame. It must contains an `value` variable and a
#'   variable `name` with the date of observations.
#' @param scale an integer, representing the time scale at which the SPEI / SPI
#'   will be computed.
#' @returns A \link[tibble]{tbl_df}, with the SPI values in wide format, i.e.
#'   each column is an observation in time.
#' @seealso [SPEI::spi]
#'
#' @examples
#' spi_wide(data, scale = 1)

spi_wide <- function(df_list, scale = 1) {
    SPEI::spi(df_list$value, scale = scale, verbose = FALSE) |>
        fitted() |>
        t() |>
        as.data.frame() |>
        setNames(df_list$name)
}
