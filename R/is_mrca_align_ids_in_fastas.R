#' Determine if an MRCA prior's alignment IDs are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if the MRCA prior's alignment IDs
#'   is present in the FASTA files.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_mrca_align_ids_in_fastas <- function(
  mrca_prior,
  fasta_filenames
) {
  check_true(is_mrca_prior(mrca_prior))
  ids <- get_alignment_ids_from_fasta_filenames(fasta_filenames)
  check_true(is_mrca_prior(mrca_prior))
  check_true(!is_one_na(mrca_prior))
  check_true("alignment_id" %in% names(mrca_prior))
  check_true(!is_one_na(mrca_prior$alignment_id))
  mrca_prior$alignment_id %in% ids
}
