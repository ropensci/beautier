#' See if x is one MRCA prior with a distribition
#' @param x the object to be tested
#' @inheritParams default_params_doc
#' @return TRUE if x is one MRCA prior with a distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
is_mrca_prior_with_distr <- function(
  x
) {
  if (is_one_na(x)) return(FALSE)
  if (!is_mrca_prior(x)) return(FALSE)
  is_distr(x$mrca_distr)
}
