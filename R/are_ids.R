#' Determine if x consists out of IDs
#' @param x the object to check if it consists out of IDs
#' @return TRUE if x, or all elements of x, are IDs
#' @author Richèl J.C. Bilderbeek
#' @seealso to check one ID, use \code{\link{is_id}}
#' @noRd
are_ids <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_id(x)) return(TRUE) # nolint beautier function
  if (!is.vector(x)) return(FALSE)
  for (i in x) {
    if (!is_id(i)) return(FALSE) # nolint beautier function
  }
  return(TRUE)
}
