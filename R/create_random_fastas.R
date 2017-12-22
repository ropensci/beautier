#' Create one or more random FASTA files
#' @inheritParams default_params_doc
#' @param n_taxa The number of taxa
#' @return Nothing, creates one or more random FASTA files
#' @author Richel J.C. Bilderbeek
create_random_fastas <- function(
  n_taxa,
  sequence_length,
  fasta_filenames
) {
  for (i in seq_along(fasta_filenames)) {
    fasta_filename <- fasta_filenames[i]
    taxa_name_ext <- paste0("_", i)
    create_random_fasta(
      n_taxa = n_taxa,
      sequence_length = sequence_length,
      fasta_filename = fasta_filename,
      taxa_name_ext = taxa_name_ext
    )
  }
  testit::assert(files_exist(fasta_filenames))
}
