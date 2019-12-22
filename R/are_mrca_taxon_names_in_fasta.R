#' Determine if the MRCA priors' taxa names are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if the MRCA priors' taxa names are
#'   present in the FASTA files. FALSE otherwise.
#' @author Richèl J.C. Bilderbeek
#' @export
are_mrca_taxon_names_in_fasta <- function(
  mrca_prior,
  fasta_filename
) {
  testit::assert(beautier::is_mrca_prior(mrca_prior))
  testit::assert(
    beautier::is_mrca_align_id_in_fasta(
      mrca_prior = mrca_prior,
      fasta_filename = fasta_filename
    )
  )
  if (beautier::get_alignment_id(fasta_filename) == mrca_prior$alignment_id) {
    for (name in mrca_prior$taxa_names) {
      if (!name %in% beautier::get_taxa_names(fasta_filename)) {
        return(FALSE)
      }
    }
  }
  TRUE
}
