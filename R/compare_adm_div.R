#' Compare hierarchical administrative names between two datasets
#'
#' This function compares sets of administrative unit names between two datasets
#' at a given hierarchical level. For each parent administrative unit at the
#' previous level, it identifies names that are present in one dataset but not
#' the other.
#'
#' The comparison is hierarchical:
#' \itemize{
#'   \item \code{level = 1}: global comparison (all observations grouped together)
#'   \item \code{level > 1}: comparison is performed within each unique
#'   combination of \code{adm_div_1, ..., adm_div_(level - 1)}
#' }
#'
#' For each parent group, the function returns:
#' \itemize{
#'   \item names present only in \code{x}
#'   \item names present only in \code{y}
#' }
#'
#' @param x A data frame or tibble containing administrative unit names.
#' Must include a column named \code{adm_div_<level>} and, if \code{level > 1},
#' columns \code{adm_div_1, ..., adm_div_(level - 1)}.
#'
#' @param y A data frame or tibble with the same structure as \code{x}.
#'
#' @param level Integer scalar. Administrative level to compare.
#' Must correspond to a column \code{adm_div_<level>}. If greater than 1,
#' grouping is performed by the full chain of ancestor columns
#' \code{adm_div_1, ..., adm_div_(level - 1)}, not just the immediate parent.
#'
#' @return A tibble with one row per parent administrative unit combination
#' containing:
#' \describe{
#'   \item{level}{The level of comparison}
#'   \item{parent}{The parent administrative unit chain, concatenated with
#'   \code{" || "} when \code{level > 1} (or \code{"ALL"} for level 1)}
#'   \item{x_only}{Character vector of names present only in \code{x}}
#'   \item{y_only}{Character vector of names present only in \code{y}}
#' }
#'
#' @details
#' The parent key is constructed by concatenating all ancestor administrative
#' levels using " || " as a separator.
#' This means two units at the same level sharing a name (e.g. two districts
#' both called "Central") will only be treated as a match if their entire
#' ancestor chain is identical — a "Central" district under Region A / Province
#' X is never pooled with a "Central" district under Region A / Province Y,
#' or under an entirely different region.
#'
#' Internally, the function:
#' \itemize{
#'   \item concatenates ancestor columns into a single \code{parent} key
#'   \item selects unique combinations of parent key and name
#'   \item removes missing values in the name column
#'   \item aggregates names per parent into sorted lists
#'   \item performs a full join between datasets at the parent level
#'   \item computes set differences using \code{setdiff()}
#' }
#'
#' The helper function \code{build_name_sets()} is used internally to construct
#' grouped name lists but is not exported.
#'
#' @examples
#' \dontrun{
#' compare_adm_div(x, y, level = 1)
#' compare_adm_div(x, y, level = 2)
#' compare_adm_div(x, y, level = 3)
#' }
#'
#' @export
compare_adm_div <- function(x, y, level = 1) {
    x <- tibble::as_tibble(x)
    y <- tibble::as_tibble(y)

    stopifnot(
        is.numeric(level),
        length(level) == 1,
        level >= 1,
        level %% 1 == 0
    )

    col <- paste0("adm_div_", level)
    parent_cols <- if (level == 1) NULL else paste0("adm_div_",
                                                    seq_len(level - 1))

    stopifnot(col %in% colnames(x))
    stopifnot(col %in% colnames(y))
    if (!is.null(parent_cols)) {
        stopifnot(all(parent_cols %in% colnames(x)))
        stopifnot(all(parent_cols %in% colnames(y)))
    }

    x_sets <- build_name_sets(x, parent_cols, col)
    y_sets <- build_name_sets(y, parent_cols, col)

    dplyr::full_join(
        x_sets,
        y_sets,
        by = "parent",
        suffix = c("_x", "_y")
    ) |>
        tidyr::replace_na(
            list(
                names_var_x = list(character()),
                names_var_y = list(character())
            )
        ) |>
        dplyr::mutate(
            level = level,
            x_only = purrr::map2(names_var_x, names_var_y, setdiff),
            y_only = purrr::map2(names_var_y, names_var_x, setdiff)
        ) |>
        dplyr::select(
            level,
            parent,
            x_only,
            y_only
        )
}

#' Build sorted name sets grouped by the full ancestor chain
#' @noRd
build_name_sets <- function(df, parent_cols, col) {
    df <- df |>
        dplyr::mutate(name_var = as.character(.data[[col]]))

    if (is.null(parent_cols)) {
        df <- df |>
            dplyr::mutate(parent = "ALL")
    } else {
        df <- df |>
            tidyr::unite("parent",
                         dplyr::all_of(parent_cols),
                         sep = " || ",
                         remove = FALSE)
    }

    df |>
        dplyr::distinct(parent, name_var) |>
        dplyr::filter(!is.na(name_var)) |>
        dplyr::summarise(
            names_var = list(sort(name_var)),
            .by = .data$parent
        )
}
