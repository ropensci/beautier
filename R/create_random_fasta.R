#' Create a random FASTA file
#' @param n_taxa The number of taxa
#' @param sequence_length The number of base pairs the alignment will have
#' @param filename the name of the FASTA file created
#' @return Nothing
#' @export
create_random_fasta <- function(
  n_taxa,
  sequence_length,
  filename) {
  if (n_taxa < 2) {
    stop("need n_taxa >= 2")
  }
  if (sequence_length < 1) {
    stop("need sequence_length >= 1")
  }
  if (!is.character(filename)) {
    stop("filename must be a character string")
  }
  if (filename == "") {
    stop("filename must have non-zero length")
  }
  alignments <- beastscriptr::create_random_alignment(n_taxa, sequence_length)
  phangorn::write.phyDat(alignments, file = filename, format = "fasta")
}
