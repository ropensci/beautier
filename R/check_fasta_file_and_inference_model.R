#' Checks if the input FASTA file and the inference model agree.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
check_fasta_file_and_inference_model <- function(
  input_filename,
  inference_model
) {
  # Higher-level checks

  # If there is a CBS tree prior, the number of groups it has
  # must be equals or more than the number of taxa in the alignment
  if (is_cbs_tree_prior(inference_model$tree_prior)) { # nolint beautier function
    n_taxa <- get_n_taxa(input_filename) # nolint beautier function
    group_sizes_dimension <- inference_model$tree_prior$group_sizes_dimension
    if (n_taxa <= group_sizes_dimension) {
      stop(
        "'group_sizes_dimension' (", group_sizes_dimension,
        ") must be less than the number of taxa (", n_taxa, ")"
      )
    }
  }

  # Be not smart for now
  mrca_priors <- list(inference_model$mrca_prior)

  # All MRCA's taxa names must be in the FASTA files
  if (!beautier::is_one_na(mrca_priors)) {
    testit::assert(beautier::are_mrca_priors(mrca_priors))
    if (!beautier::are_mrca_align_ids_in_fastas(
        mrca_priors = mrca_priors,
        fasta_filenames = input_filename
      )
    ) {
      mrca_ids <- NULL
      for (mrca_prior in mrca_priors) {
        mrca_ids <- paste(mrca_ids, mrca_prior$alignment_id)
      }

      stop(
        paste0(
          "All MRCA prior's alignment IDs must match the FASTA file IDs. ",
          "Use 'get_alignment_id' on the FASTA filename ",
          "to get the correct alignment ID. ",
          "Alignment IDs: ",
            get_alignment_ids_from_fasta_filenames(input_filename),
          ". MRCA alignment IDs: ", mrca_ids
        )
      )
    }

    if (!beautier::are_mrca_taxa_names_in_fastas(
        mrca_priors = mrca_priors, fasta_filenames = input_filename
      )
    ) {
      stop("All MRCA prior's taxa names must be FASTA file taxa names")
    }
  }

  if (!beautier::are_mrca_taxa_non_intersecting(mrca_priors)) {
    stop("Monophyletic MRCA priors must have taxon sets without intersection")
  }

}
