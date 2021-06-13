#' See if the inference model has one MRCA prior with a distribution
#' @inheritParams default_params_doc
#' @return TRUE if the inference model has one MRCA prior with a distribution,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
has_mrca_prior_with_distr <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  if (!has_mrca_prior(inference_model)) return(FALSE)
  beautier::is_mrca_prior_with_distr(inference_model$mrca_prior)
}
