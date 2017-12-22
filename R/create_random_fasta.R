#' Create a random FASTA file
#' @inheritParams default_params_doc
#' @param n_taxa The number of taxa
#' @param taxa_name_ext the extension of the taxa names
#' @return Nothing, creates a FASTA file
#' @author Richel J.C. Bilderbeek
create_random_fasta <- function(
  n_taxa,
  sequence_length,
  fasta_filename,
  taxa_name_ext = ""
) {
  if (n_taxa < 2) {
    stop("need n_taxa >= 2")
  }
  if (sequence_length < 1) {
    stop("need sequence_length >= 1")
  }
  if (!is.character(fasta_filename)) {
    stop("filename must be a character string")
  }
  if (fasta_filename == "") {
    stop("filename must have non-zero length")
  }
  alignments <- create_random_alignment(
    n_taxa,
    sequence_length,
    taxa_name_ext = taxa_name_ext
  )
  phangorn::write.phyDat(alignments, file = fasta_filename, format = "fasta")
}
