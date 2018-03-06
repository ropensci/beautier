#' Determine if x consists out of mrca_priors objects
#' @param x the object to check if it consists out of mrca_priors objects
#' @return TRUE if x, or all elements of x, are mrca_prior objects
#' @author Richel J.C. Bilderbeek
are_mrca_priors <- function(
  x
) {
  if (length(x) == 1 && is.na(x)) return(TRUE)
  if (is.null(x)) return(FALSE)
  if (is_mrca_prior(x)) return(TRUE)
  for (i in x) {
    if (!is_mrca_prior(i)) return(FALSE)
  }
  TRUE
}