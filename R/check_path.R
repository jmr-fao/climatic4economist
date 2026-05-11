#' Build and validate a file path
#'
#' Wrapper around [base::file.path()] that progressively checks whether
#' each component of the path exists. If the full path does not exist,
#' the function identifies the first missing element and optionally lists
#' the contents of the last valid directory.
#'
#' @param ... Character vectors containing path components passed to
#'   [base::file.path()].
#' @param list_contents Logical. If `TRUE` (default), list the contents
#'   of the last existing directory when validation fails.
#'
#' @returns
#' A character string containing the validated file path.
#'
#' @details
#' The function progressively checks each level of the constructed path:
#'
#' \preformatted{
#' folder/
#' folder/subfolder/
#' folder/subfolder/file.txt
#' }
#'
#' If one level does not exist, the function stops and reports:
#'
#' - the first missing path component
#' - the last valid directory
#' - optionally, the files and folders available there
#'
#' This is useful for debugging typos or incorrect file structures.
#'
#' @examples
#' \dontrun{
#'
#' check_path(
#'   "project",
#'   "data",
#'   "geo",
#'   "file.txt"
#' )
#'
#' }
#'
#' @export
check_path <- function(..., list_contents = TRUE) {

    # Build full path
    full_path <- base::file.path(...)

    # Return immediately if path exists
    if (base::file.exists(full_path)) {
        return(full_path)
    }

    # Store path components
    parts <- unlist(list(...))

    # Build cumulative paths
    cumulative_paths <- purrr::accumulate(parts,
                                          base::file.path)

    # Check existence progressively
    exists_vec <- base::file.exists(cumulative_paths)

    # First missing element
    first_missing <- which(!exists_vec)[1]

    # Base error message
    msg <- paste0("Path does not exist.\n",
                  "Problem occurred at:\n  ",
                  cumulative_paths[first_missing])

    # No valid folder found
    if (first_missing == 1) {
        stop(msg, call. = FALSE)
    }

    # Last valid folder
    last_valid <- cumulative_paths[first_missing - 1]

    # Optionally list available files/folders
    if (list_contents) {
        contents <- base::list.files(last_valid,
                                     all.files = TRUE)

        msg <- paste0(msg,
                      "\n\nLast existing directory:\n  ",
                      last_valid,
                      "\n\nAvailable files and folders:\n  ",
                      paste(contents, collapse = "\n  "))
    }

    stop(msg, call. = FALSE)
}
