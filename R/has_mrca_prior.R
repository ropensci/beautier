#' Determines if the inference model has an MRCA prior.
#'
#' Will \link{stop} if the inference model is invalid
#' @inheritParams default_params_doc
#' @return TRUE if the inference model has an MRCA prior,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
has_mrca_prior <- function(
  inference_model
) {
  check_inference_model(inference_model) # nolint beautier function
  !is_one_na(inference_model$mrca_prior) # nolint beautier function
}
