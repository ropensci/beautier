#' Determine if x consists out of initialized MRCA priors
#' @param x the object to check if it consists out of
#'   initialized MRCA priors
#' @return TRUE if x, or all elements of x, are initialized MRCA priors
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_init_mrca_priors <- function(
  x
) {
  if (!are_mrca_priors(x)) return(FALSE) # nolint beautier function
  for (i in x) {
    if (!is_init_mrca_prior(i)) return(FALSE) # nolint beautier function call
  }
  return(TRUE)
}
