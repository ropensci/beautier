#' Determine if two treelogs are equal.
#'
#' Will \link{stop} if the arguments are not treelogs.
#' @param treelog_1 an treelog, as created by \link{create_treelog}
#' @param treelog_2 an treelog, as created by \link{create_treelog}
#' @return TRUE if the two treelogs are equal
#' @seealso Use \link{create_treelog} to create an treelog
#' @examples
#' check_empty_beautier_folder()
#'
#' treelog_1 <- create_treelog(log_every = 1000)
#' treelog_2 <- create_treelog(log_every = 314)
#' # TRUE
#' are_equal_treelogs(treelog_1, treelog_1)
#' # FALSE
#' are_equal_treelogs(treelog_1, treelog_2)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_treelogs <- function(
  treelog_1, treelog_2
) {
  beautier::check_treelog(treelog_1)
  beautier::check_treelog(treelog_2)
  # Can be both NA
  if (is.na(treelog_1$filename)) {
    if (!is.na(treelog_2$filename)) return(FALSE)
  } else {
    if (treelog_1$filename != treelog_2$filename) return(FALSE)
  }
  treelog_1$log_every == treelog_2$log_every &&
    treelog_1$mode == treelog_2$mode &&
    treelog_1$sanitise_headers == treelog_2$sanitise_headers &&
    treelog_1$sort == treelog_2$sort
}
