#' Determine if x consists out of IDs
#' @param x the object to check if it consists out of IDs
#' @return TRUE if x, or all elements of x, are IDs
#' @author Richel J.C. Bilderbeek
#' @export
are_ids <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (beautier::is_id(x)) return(TRUE)
  if (!is.vector(x)) return(FALSE)
  for (i in x) {
    if (!beautier::is_id(i)) return(FALSE)
  }
  return(TRUE)
}
