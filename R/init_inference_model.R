#' Initialize an inference model
#' @inheritParams default_params_doc
#' @export
init_inference_model <- function(input_filename, inference_model) {

  # Set the alignment ID and taxon names
  inference_model <- init_mrca_prior(input_filename, inference_model)

  inference_model
}
