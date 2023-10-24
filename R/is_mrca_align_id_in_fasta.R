#' Determine if an MRCA prior's alignment IDs is present in the FASTA file
#' @inheritParams default_params_doc
#' @return TRUE if the MRCA prior's alignment IDs
#'   is present in the FASTA file.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_mrca_align_id_in_fasta <- function(
  mrca_prior,
  fasta_filename
) {
  check_true(is_mrca_prior(mrca_prior))
  id <- get_alignment_id(fasta_filename)
  check_true(is_mrca_prior(mrca_prior))
  check_true(!is_one_na(mrca_prior))
  check_true("alignment_id" %in% names(mrca_prior))
  check_true(!is_one_na(mrca_prior$alignment_id))
  mrca_prior$alignment_id == id
}
