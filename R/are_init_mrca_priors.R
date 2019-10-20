#' Determine if x consists out of initialized MRCA priors
#' @param x the object to check if it consists out of
#'   initialized MRCA priors
#' @return TRUE if x, or all elements of x, are initialized MRCA priors
#' @author Richèl J.C. Bilderbeek
#' @export
are_init_mrca_priors <- function(
  x
) {
  if (!beautier::are_mrca_priors(x)) return(FALSE)
  for (i in x) {
    if (!beautier::is_init_mrca_prior(i)) return(FALSE)
  }
  return(TRUE)
}
