#' Compute Water Balance
#'
#' This function calculates the water balance by summing precipitation (`pre`) and
#' potential evapotranspiration (`pet`) for each corresponding cell.
#'
#' @param pre A data frame containing precipitation values with ID, x_cell, and y_cell columns.
#' @param pet A data frame containing potential evapotranspiration values with the same structure as `pre`.
#'
#' @return A data frame with the same structure as `pre`, where the values represent the water balance.
#'
#' @export
#'
#' @examples
#' pre <- data.frame(ID = 1:3, x_cell = c(10, 20, 30), y_cell = c(40, 50, 60), `2020` = c(100, 120, 140))
#' pet <- data.frame(ID = 1:3, x_cell = c(10, 20, 30), y_cell = c(40, 50, 60), `2020` = c(-50, -60, -70))
#' compute_water_balance(pre, pet)

compute_water_balance <- function(pre, pet) {
    id_vars <- grep("ID|x_cell|y_cell|coverage|adm_div", names(pre))
    date_vars <- grep("[0-9]{4}", names(pre))

    # drop = FALSE keeps the date column names when there is a single period
    water_balance <- pre[, date_vars, drop = FALSE] + pet[, date_vars, drop = FALSE]
    cbind(pre[, id_vars, drop = FALSE], water_balance)
}


