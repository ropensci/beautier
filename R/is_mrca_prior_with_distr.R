#' See if x is one MRCA prior with a distribution
#' @param x the object to be tested
#' @return TRUE if x is one MRCA prior with a distribution,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_mrca_prior_with_distr <- function(
  x
) {
  if (beautier::is_one_na(x)) return(FALSE)
  if (!beautier::is_mrca_prior(x)) return(FALSE)
  beautier::is_distr(x$mrca_distr)
}
