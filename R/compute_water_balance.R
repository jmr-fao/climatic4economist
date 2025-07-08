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
    id_vars <- grep("ID|x_cell|y_cell|coverage", names(pre))

    water_balance <- pre[, -id_vars] + pet[, -id_vars]
    cbind(pre[, id_vars], water_balance)
}

