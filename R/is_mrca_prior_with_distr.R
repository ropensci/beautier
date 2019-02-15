#' See if x is one MRCA prior with a distribition
#' @param x the object to be tested
#' @inheritParams default_params_doc
#' @return TRUE if x is one MRCA prior with a distribution,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_mrca_prior_with_distr <- function(
  x
) {
  if (is_one_na(x)) return(FALSE) # nolint beautier function
  if (!is_mrca_prior(x)) return(FALSE) # nolint beautier function
  is_distr(x$mrca_distr) # nolint beautier function
}
