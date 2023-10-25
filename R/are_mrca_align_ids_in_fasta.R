#' Determine if the MRCA priors' alignment IDs are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if all the MRCA priors' alignment IDs
#'   are present in the FASTA files.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
are_mrca_align_ids_in_fasta <- function(
  mrca_prior,
  fasta_filename
) {
  check_true(is_mrca_prior(mrca_prior))
  if (!is_mrca_align_id_in_fasta(mrca_prior, fasta_filename)) {
    return(FALSE)
  }
  TRUE
}
