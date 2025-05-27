#' Merge Data Frames or Lists by Common Variable(s)
#'
#' This function merges two data frames or lists by their common variable(s). It uses
#' \code{find_merge_var} to identify the shared columns between the two inputs and
#' performs a full join using \code{dplyr::full_join}.
#'
#' @param x A data frame or a list. If it's a list, the function will attempt to identify
#'   the common columns across all list elements.
#' @param y A data frame (optional). If provided, the function will merge \code{x} and \code{y}
#'   by the common column(s). If \code{y} is \code{NULL} and \code{x} is a list, the function
#'   will merge the elements of \code{x} based on shared column names.
#'
#' @return A merged data frame or list. If the inputs are data frames, it returns a merged
#'   data frame. If the inputs are lists, it returns the merged list elements based on the common columns.
#'
#' @seealso \code{\link[dplyr]{full_join}}, \code{\link{find_merge_var}}
#'
#' @export
#'
#' @examples
#' # Example 1: Merging two data frames with a common column
#' df1 <- data.frame(ID = 1:3, Value = c(10, 20, 30))
#' df2 <- data.frame(ID = c(2, 3, 4), Value = c(200, 300, 400))
#' merged_df <- merge_by_common(df1, df2)
#'
#' # Example 2: Merging a list of data frames by common columns
#' list_df <- list(
#'     data.frame(ID = 1:3, Value = c(10, 20, 30)),
#'     data.frame(ID = c(2, 3, 4), Value = c(200, 300, 400))
#' )
#' merged_list <- merge_by_common(list_df)

merge_by_common <- function(x, y = NULL) {
    merge_var <- find_merge_var(x, y)
    dplyr::full_join(x, y, by = merge_var)
}


#' Find Common Column(s) Between Two Data Frames or List Elements
#'
#' This function identifies the common column names between two data frames, or if a list is
#' provided, finds the common column names across all elements of the list.
#'
#' @param x A data frame or a list. If it's a list, the function will find the common column
#'   names across the elements of the list.
#' @param y A data frame (optional). If provided, the function finds the common column names
#'   between \code{x} and \code{y}.
#'
#' @return A character vector containing the names of the common column(s) between \code{x} and
#'   \code{y}, or among the elements of \code{x} if it is a list.
#'
#' @export
#'
#' @examples
#' # Example 1: Finding the common column between two data frames
#' df1 <- data.frame(ID = 1:3, Value = c(10, 20, 30))
#' df2 <- data.frame(ID = c(2, 3, 4), Value = c(200, 300, 400))
#' common_var <- find_merge_var(df1, df2)
#'
#' # Example 2: Finding common columns in a list of data frames
#' list_df <- list(
#'     data.frame(ID = 1:3, Value = c(10, 20, 30)),
#'     data.frame(ID = c(2, 3, 4), Value = c(200, 300, 400))
#' )
#' common_var_list <- find_merge_var(list_df)

find_merge_var <- function(x, y = NULL) {
    if (is.null(y) && is.list(x)) {
        out <- lapply(x, colnames) |>
            purrr::reduce(intersect)
        return(out)
    } else {
        return( intersect(names(x), names(y)) )
    }
}
