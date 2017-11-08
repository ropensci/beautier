#' Determine if x consists out of initialized tree_priors objects
#' @param x the object to check if it consists out of
#'   initialized tree_priors objects
#' @return TRUE if x, or all elements of x, are initialized tree_prior objects
#' @author Richel J.C. Bilderbeek
are_initialized_tree_priors <- function(
  x
) {
  if (!beautier::are_tree_priors(x)) return(FALSE)
  for (i in x) {
    if (!is_initialized_tree_prior(i)) return(FALSE)   # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  }
  return(TRUE)
}
