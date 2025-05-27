#' Prepare ID for unique occurrences
#'
#' This this function adds an `ID` columns that identifies unique occurrence
#' based on unique coordinates.
#'
#' @param df A data frame
#' @param lat_var <data-masking> variable with the latitude coordinate
#' @param lon_var <data-masking> variable with the longitude coordinate
#'
#' @returns The same data frame `df` with an additional `ID` variable that
#'   identifies the unique locations.
#'
#' @export
#'
#' @examples
#' prepare_coord(survey, gps_lat, gps_lon)

prepare_coord <- function(df, lat_var, lon_var) {
    df |>
        dplyr::arrange({{lat_var}}, {{lon_var}}) |>
        dplyr::group_by({{lat_var}}, {{lon_var}}) |>
        dplyr::mutate(ID = as.character(dplyr::cur_group_id()),
                      .before = 1) |>
        dplyr::ungroup()
}
