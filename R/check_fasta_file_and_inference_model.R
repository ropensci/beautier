#' Checks if the input FASTA file and the inference model agree.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
check_fasta_file_and_inference_model <- function(
  input_filename,
  inference_model
) {
  # Higher-level checks

  fasta_filename <- input_filename
  tree_prior <- inference_model$tree_prior
  if (is_cbs_tree_prior(tree_prior)) { # nolint beautier function
    n_taxa <- get_n_taxa(fasta_filename) # nolint beautier function
    group_sizes_dimension <- tree_prior$group_sizes_dimension
    if (n_taxa <= group_sizes_dimension) {
      stop(
        "'group_sizes_dimension' (", group_sizes_dimension,
        ") must be less than the number of taxa (", n_taxa, ")"
      )
    }
  }
}
