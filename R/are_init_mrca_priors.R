#' Determine if x consists out of initialized mrca_priors objects
#' @param x the object to check if it consists out of
#'   initialized mrca_priors objects
#' @return TRUE if x, or all elements of x, are initialized mrca_prior objects
#' @author Richel J.C. Bilderbeek
are_init_mrca_priors <- function(
  x
) {
  if (!are_mrca_priors(x)) return(FALSE)
  for (i in x) {
    if (!is_init_mrca_prior(i)) return(FALSE)   # nolint internal function call
  }
  return(TRUE)
}
