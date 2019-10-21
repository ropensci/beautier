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
}
