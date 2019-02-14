#' Determines if the inference model has an MRCA prior
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
has_mrca_prior <- function(
  inference_model
) {
  check_inference_model(inference_model)
  !is_one_na(inference_model$mrca_prior)
}
