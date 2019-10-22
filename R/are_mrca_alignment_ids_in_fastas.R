#' Determine if the MRCA priors' alignment IDs are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if all the MRCA priors' alignment IDs
#'   are present in the FASTA files.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
are_mrca_align_ids_in_fastas <- function(
  mrca_priors,
  fasta_filenames
) {
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  for (mrca_prior in mrca_priors) {
    if (!is_mrca_align_ids_in_fastas(mrca_prior, fasta_filenames)) {
      return(FALSE)
    }
  }
  TRUE
}
