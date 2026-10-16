#' Find the column identifying the units of a table
#'
#' Most functions in the package work on one row per unit and per date, keyed by
#' a column naming the unit. That column is called `ID` when the units come from
#' [prepare_coord()] and `ID_adm_div` when they come from [read_GAUL()] or
#' [read_geoBoundaries()], so the key is looked up rather than assumed.
#'
#' Auto-detection is what lets the key travel through a pipeline: each function
#' finds whichever key its input carries, so a table keyed by `ID_adm_div` runs
#' end to end without naming the column at any step. Pass `id` when the table
#' carries more than one candidate, or when the key is named something else
#' entirely.
#'
#' @param x A data frame, or anything else with `names()`, such as a
#'   [terra::SpatVector].
#' @param id Optional character string naming the key column. When `NULL`, the
#'   first of `candidates` present in `x` is used.
#' @param candidates Character vector of column names to look for, in order of
#'   preference.
#' @param arg Name of the calling function's argument, used in error messages so
#'   they point at the argument the caller can actually set.
#'
#' @return A single character string, the name of the key column.
#'
#' @noRd
resolve_key <- function(x, id = NULL, candidates = c("ID", "ID_adm_div"),
                        arg = "id") {

    if (!is.null(id)) {
        if (!is.character(id) || length(id) != 1L || is.na(id)) {
            stop("`", arg, "` must be a single column name, given as a string.",
                 call. = FALSE)
        }
        if (!id %in% names(x)) {
            stop("Column `", id, "` not found. Available columns: ",
                 paste0("`", names(x), "`", collapse = ", "), ".",
                 call. = FALSE)
        }
        return(id)
    }

    found <- intersect(candidates, names(x))

    if (length(found) == 0L) {
        stop("No valid ID column found. Looked for ",
             paste0("`", candidates, "`", collapse = " and "),
             ". Provide `", arg, "` explicitly.", call. = FALSE)
    }

    found[1L]
}
