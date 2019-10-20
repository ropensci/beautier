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
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  ids <- get_alignment_ids_from_fasta_filenames(fasta_filenames) # nolint beautier function
  for (mrca_prior in mrca_priors) {
    testit::assert(beautier::is_mrca_prior(mrca_prior))
    testit::assert(!beautier::is_one_na(mrca_prior))
    testit::assert("alignment_id" %in% names(mrca_prior))
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    if (!mrca_prior$alignment_id %in% ids) {
      return(FALSE)
    }
  }
  TRUE
}
