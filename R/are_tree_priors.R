#' Determine if x consists out of tree_priors objects
#' @param x the object to check if it consists out of tree_priors objects
#' @return TRUE if x, or all elements of x, are tree_prior objects
#' @author Richel J.C. Bilderbeek
are_tree_priors <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_tree_prior(x)) {
    return(TRUE)
  }
  for (i in x) {
    if (!is_tree_prior(i)) return(FALSE)
  }
  return(TRUE)
}
