#' Create one or more random FASTA files
#' @inheritParams default_params_doc
#' @param n_taxa The number of taxa
#' @return Nothing, creates a FASTA file
#' @author Richel J.C. Bilderbeek
create_random_fastas <- function(
  n_taxa,
  sequence_length,
  fasta_filenames
) {
  for (fasta_filename in fasta_filenames) {
    create_random_fasta(
      n_taxa = n_taxa,
      sequence_length = sequence_length,
      fasta_filename = fasta_filename
    )
  }
}
