#' Initialize an inference model
#' @inheritParams default_params_doc
#' @export
init_inference_model <- function(input_filename, inference_model) {

  # Site model. TODO: remove plurals
  inference_model$site_model <- init_site_models(
    site_models = list(inference_model$site_model),
    ids = get_alignment_ids_from_fasta_filenames(
      fasta_filenames = input_filename
    ),
    distr_id = 0,
    param_id = 0
  )[[1]]

  # Clock model. TODO: remove plurals
  inference_model$clock_model <- init_clock_models(
    clock_models = list(inference_model$clock_model),
    fasta_filenames = input_filename,
    distr_id = 0 + get_site_models_n_distrs(list(inference_model$site_model)),
    param_id = 0 + get_site_models_n_params(list(inference_model$site_model))
  )[[1]]

  # Tree prior. TODO: remove plurals
  inference_model$tree_prior <- beautier::init_tree_priors(
    list(inference_model$tree_prior),
    ids = get_alignment_ids_from_fasta_filenames(
      fasta_filenames = input_filename
    ),
    distr_id = 100,
    param_id = 200
  )[[1]]

  # MRCA prior. TODO: remove plurals
  inference_model$mrca_prior <- beautier::init_mrca_priors(
    list(inference_model$mrca_prior),
    distr_id = 150,
    param_id = 300
  )[[1]]

  # Set the alignment ID and taxon names
  inference_model <- beautier::init_mrca_prior(input_filename, inference_model)

  inference_model
}
