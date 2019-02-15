#' Determine if the MRCA priors' alignment IDs are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if all the MRCA priors' alignment IDs
#'   are present in the FASTA files.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_mrca_align_ids_in_fastas <- function(
  mrca_priors,
  fasta_filenames
) {
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  ids <- get_alignment_ids(fasta_filenames) # nolint beautier function
  for (mrca_prior in mrca_priors) {
    testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
    testit::assert(!is_one_na(mrca_prior)) # nolint beautier function
    testit::assert("alignment_id" %in% names(mrca_prior))
    testit::assert(!is_one_na(mrca_prior$alignment_id)) # nolint beautier function
    if (!mrca_prior$alignment_id %in% ids) { # nolint beautier function
      return(FALSE)
    }
  }
  TRUE
}
