#' Determine if x consists out of MRCA priors
#' @param x the object to check if it consists out of MRCA priors
#' @return TRUE if x, or all elements of x, are MRCA priors
#' @author Richel J.C. Bilderbeek
#' @noRd
are_mrca_priors <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_mrca_prior(x)) return(TRUE) # nolint internal function
  for (i in x) {
    if (!is_mrca_prior(i)) return(FALSE) # nolint internal function
  }
  TRUE
}
