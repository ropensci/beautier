#' Checks if the input FASTA file and the inference model agree.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_file_and_model_agree <- function(
  input_filename,
  inference_model
) {
  # Higher-level checks

  # If there is a CBS tree prior, the number of groups it has
  # must be equals or more than the number of taxa in the alignment
  if (beautier::is_cbs_tree_prior(inference_model$tree_prior)) {
    n_taxa <- beautier::get_n_taxa(input_filename)
    group_sizes_dimension <- inference_model$tree_prior$group_sizes_dimension
    if (n_taxa <= group_sizes_dimension) {
      stop(
        "'group_sizes_dimension' (", group_sizes_dimension,
        ") must be less than the number of taxa (", n_taxa, ")"
      )
    }
  }

  # All MRCA prior's alignment IDs must match the FASTA file IDs
  if (!beautier::is_one_na(inference_model$mrca_prior)) {
    testit::assert(beautier::is_mrca_prior(inference_model$mrca_prior))

    if (!beautier::is_mrca_align_id_in_fasta(
        mrca_prior = inference_model$mrca_prior,
        fasta_filename = input_filename
      )
    ) {
      stop(
        paste0(
          "All MRCA prior's alignment IDs must match the FASTA file IDs. ",
          "Use 'get_alignment_id' on the FASTA filename ",
          "to get the correct alignment ID. ",
          "Alignment IDs: ",
            beautier::get_alignment_id(input_filename),
          ". MRCA alignment ID: ", inference_model$mrca_prior$alignment_id
        )
      )
    }
  }

  # All MRCA prior's taxa names must be FASTA file taxa names
  if (!beautier::is_one_na(inference_model$mrca_prior)) {
    if (!beautier::are_mrca_taxon_names_in_fasta(
        mrca_prior = inference_model$mrca_prior,
        fasta_filename = input_filename
      )
    ) {
      stop("All MRCA prior's taxa names must be FASTA file taxa names")
    }
  }
}
