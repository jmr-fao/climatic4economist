#' Find a Root Directory by Walking Upward
#'
#' Starting from a given path (default: current working directory), this function
#' walks up the directory tree until it finds a folder matching one of the specified names.
#'
#' @param start Character. Starting path (default = current working directory).
#' @param root_directories Character vector. Names of folders to match (case-insensitive by default).
#' @param ignore_case Logical. If TRUE, the search is case-insensitive (default = TRUE).
#'
#' @return Character path to the found root directory, or `NULL` if not found.
#'
#' @examples
#' \dontrun{
#' find_root_directory(start = getwd(), root_directories = c("OneDirive", ".git"))
#' }
#'
#' @export
#'
find_root_directory <- function(start = getwd(),
                                root_directories,
                                ignore_case = TRUE) {
    current <- normalizePath(start, winslash = "/")
    repeat {
        base <- basename(current)

        if (ignore_case) {
            if (tolower(base) %in% tolower(root_directories)) {
                return(current)
            }
        } else {
            if (base %in% root_directories) {
                return(current)
            }
        }

        parent <- dirname(current)
        if (parent == current) {  # reached filesystem root
            stop(paste("Folder", root_directory, "not found"))
        }

        current <- parent
    }
}
