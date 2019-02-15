#' Determine if the MRCA priors' taxa names are present in the FASTA files
#' @inheritParams default_params_doc
#' @return TRUE if the MRCA priors' taxa names are
#'   present in the FASTA files. FALSE otherwise.
#' @author Richèl J.C. Bilderbeek
#' @noRd
are_mrca_taxa_names_in_fastas <- function(
  mrca_priors,
  fasta_filenames
) {
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  testit::assert(
    are_mrca_align_ids_in_fastas( # nolint beautier function
      mrca_priors = mrca_priors,
      fasta_filenames = fasta_filenames
    )
  )
  for (mrca_prior in mrca_priors) {
    for (fasta_filename in fasta_filenames) {
      if (get_alignment_id(fasta_filename) == mrca_prior$alignment_id) {
        for (name in mrca_prior$taxa_names) {
          if (!name %in% get_taxa_names(fasta_filename)) {
          return(FALSE)
          }
        }
      }
    }
  }
  TRUE
}
