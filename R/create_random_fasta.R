#' Create a random FASTA file
#' @param n_taxa The number of taxa
#' @param sequence_length The number of base pairs the alignment will have
#' @param rate mutation rate
#' @param filename the name of the FASTA file created
#' @return Nothing
#' @export
create_random_fasta <- function(
  n_taxa,
  sequence_length,
  filename) {
  if (n_taxa < 2) {
    stop("create_random_fasta: ",
         "need n_taxa >= 2, ",
         "instead of ", n_taxa
    )
  }
  if (n_taxa < 2) {
    stop("create_random_fasta: ",
         "need sequence_length >= 1, ",
         "instead of ", sequence_length
    )
  }
  if (!is.character(filename)) {
    stop("create_random_fasta: ",
         "filename must be a character string"
    )
  }
  if (filename == "") {
    stop("create_random_fasta: ",
         "filename must have non-zero length"
    )
  }
  alignments <- beastscriptr::create_random_alignment(n_taxa, sequence_length)
  phangorn::write.phyDat(alignments, file = filename, format = "fasta")
}
