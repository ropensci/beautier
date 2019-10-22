#' Initialize an inference model
#' @inheritParams default_params_doc
#' @export
init_inference_model <- function(input_filename, inference_model) {

  inference_model$site_model <- init_site_models(
    site_models = list(inference_model$site_model),
    ids = get_alignment_ids_from_fasta_filenames(
      fasta_filenames = input_filename
    ),
    distr_id = 0,
    param_id = 0
  )[[1]]

  # Set the alignment ID and taxon names
  inference_model <- init_mrca_prior(input_filename, inference_model)

  inference_model
}
