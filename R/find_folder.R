#' Find a Subfolder Within a Root Directory
#'
#' Searches for a subfolder with a given name starting from a specified root
#' directory. The function first locates the root directory using
#' `climatic4economist::find_root_directory()` and then recursively searches
#' for the target subfolder using
#' `climatic4economist::find_subfolder()`.
#'
#' @param folder_name A character string giving the name of the subfolder to
#'   search for.
#' @param root A character string specifying the name of the root directory
#'   from which the search should begin. Defaults to `"Users"`.
#'
#' @return
#' A character string containing the full path to the first matching sub folder.
#' The function returns `NULL` (or the value returned by
#' `climatic4economist::find_subfolder()`) if no matching folder is found.
#'
#' @examples
#' \dontrun{
#' find_folder("Documents")
#'
#' find_folder(
#'   folder_name = "sp_repository",
#'   root = "Users"
#' )
#' }
#' @export

find_folder <- function(folder_name, root = "Users") {
    climatic4economist::find_root_directory(
        root_directories = root
    ) |>
        climatic4economist::find_subfolder(
            target = folder_name
        )
}

