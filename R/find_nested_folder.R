#' Find a Nested Subfolder
#'
#' Recursively locates a sequence of nested folders. The search begins by
#' locating the first folder from a specified root directory using
#' `find_folder()`. Each subsequent folder is then searched recursively within
#' the previously located folder using
#' `climatic4economist::find_subfolder()`.
#'
#' This hierarchical search reduces the risk of matching folders with the same
#' name elsewhere in the file system.
#'
#' @param ... Character strings giving the ordered sequence of folder names to
#'   locate. The first folder is searched for from `root`; each subsequent
#'   folder is searched for within the previously located folder.
#' @param root A character string specifying the name of the root directory
#'   from which the search begins. Defaults to `"Users"`.
#'
#' @return
#' A character string containing the full path to the final folder in the
#' sequence.
#'
#' @examples
#' \dontrun{
#' # Find "adm_div" within "sp_repository"
#' find_nested_folder(
#'   "sp_repository",
#'   "adm_div"
#' )
#'
#' # Find a deeper folder hierarchy
#' find_nested_folder(
#'   "project",
#'   "data",
#'   "raw",
#'   "rasters"
#' )
#' }
#'
#' @export

find_nested_folder <- function(..., root = "Users") {

    folders <- c(...)

    path <- find_folder(
        folder_name = folders[1],
        root = root
    )

    if (length(folders) > 1) {
        for (folder in folders[-1]) {
            path <- find_subfolder(
                root = path,
                target = folder
            )
        }
    }

    path
}
