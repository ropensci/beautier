#' Initialize the MRCA prior.
#'
#' Initialized by
#' \itemize{
#'   \item if no alignment ID is set,
#'     it is set by reading it from the alignment file
#'   \item if no taxa names are set,
#'     these are set by reading these from the alignment file
#' }
#' @inheritParams default_params_doc
init_mrca_prior <- function(
  input_filename,
  inference_model
) {
  # Fill in MRCA prior's taxa names and alignment ID if those are NA
  if (!beautier::is_one_na(inference_model$mrca_prior)) {
    if (beautier::is_one_na(inference_model$mrca_prior$alignment_id)) {
      inference_model$mrca_prior$alignment_id <- get_alignment_id(input_filename) # nolint beautier function
    }
    if (beautier::is_one_na(inference_model$mrca_prior$taxa_names)) {
      inference_model$mrca_prior$taxa_names <- get_taxa_names(input_filename) # nolint beautier function
    }
  }
  inference_model
}