#' Determine if x consists out of IDs
#' @param x the object to check if it consists out of IDs
#' @return TRUE if x, or all elements of x, are IDs
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' are_ids("anthus_aco")
#' are_ids(c("anthus_aco", "anthus_nd2"))
#' are_ids(list("anthus_aco", "anthus_nd2"))
#' are_ids(c(1, 2))
#' are_ids(1)
#'
#' # FALSE
#' are_ids(NULL)
#' are_ids(NA)
#' are_ids(c())
#' are_ids(ape::rcoal(3))
#' are_ids(c(ape::rcoal(3), ape::rcoal(4)))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @seealso to check one ID, use \link{is_id}
#' @export
are_ids <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_id(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)
  for (i in x) {
    if (!is_id(i)) return(FALSE)
  }
  return(TRUE)
}
