#' Extract the number of taxa from a file
#' @param filename name of a FASTA file
#' @return the number of taxa
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_n_taxa <- function(filename) {
  check_file_exists(filename, "filename") # nolint beautier function
  length(seqinr::read.fasta(filename))
}
