#' Extract the number of taxa from a file
#' @param filename name of a FASTA file
#' @return the number of taxa
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_n_taxa <- function(filename) {

  if (!file.exists(filename)) {
    stop(
      "'filename' must be the name of a file that is present. ",
      "File '", filename, "' not found"
    )
  }
  length(seqinr::read.fasta(filename))
}
