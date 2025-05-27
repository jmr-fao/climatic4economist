#' Compute spell length for consecutive days
#'
#' @param condition A boolean vector indicating threshold exceedance.
#' @param threshold A numeric value specifying the minimum spell length (default
#'   is 2).
#'
#' @return A numeric vector indicating the length of spells where conditions
#'   hold.
#'
#' @export

compute_spell <- function(condition, threshold = 2) {
    # find consecutive days
    cosecutive_day <- sequence(rle(condition)$length)
    # remove consecutive not satisfying the condition
    cosecutive_day = dplyr::if_else(condition,
                                    cosecutive_day,
                                    NA_integer_)
    # keep only consecutive longer than the threshold
    spell = dplyr::if_else(cosecutive_day >= threshold,
                           cosecutive_day,
                           NA_integer_)
    # keep only last value of the consecutive days,
    # i.e., the actual length of the spell
    spell = dplyr::if_else(is.na(dplyr::lead(spell)),
                           spell,
                           NA_integer_)
    return(spell)
}
