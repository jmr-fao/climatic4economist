#' Find a Subfolder by Name
#'
#' Recursively searches for a subfolder within a root directory up to a given depth.
#'
#' @param root Character. Path to the root directory to start searching from.
#' @param target Character. Substring or regex pattern to match folder names.
#' @param max_depth Integer. Maximum depth of search (default = 10).
#' @param ignore_case Logical. If TRUE, search is case-insensitive (default = TRUE).
#' @param first_only Logical. If TRUE, return only the first match (default = TRUE).
#'
#' @return Character vector of matching folder paths. If no match is found, returns `NULL`.
#'
#' @examples
#' \dontrun{
#' find_subfolder("data", "weather")   # find "weather" folder under "data"
#' find_subfolder("data", "temp", max_depth = 5, first_only = FALSE)
#' }
#'
#' @export
find_subfolder <- function(root, target, max_depth = 10, ignore_case = TRUE, first_only = TRUE) {
    if (!dir.exists(root)) {
        stop("Root directory does not exist: ", root)
    }

    current_level <- root
    all_matches <- character(0)

    for (depth in seq_len(max_depth)) {
        dirs <- list.dirs(current_level, recursive = FALSE, full.names = TRUE)

        if (ignore_case) {
            match <- dirs[stringr::str_detect(dirs, regex(target, ignore_case = TRUE))]
        } else {
            match <- dirs[stringr::str_detect(dirs, target)]
        }

        if (length(match) > 0) {
            if (first_only) return(match[1])
            all_matches <- c(all_matches, match)
        }

        current_level <- dirs
    }

    if (length(all_matches) > 0) {
        return(all_matches)
    } else {
        return(NULL)
    }
}
