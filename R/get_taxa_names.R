#' Extract the names of taxa from a file
#' @param filename name of a FASTA file
#' @return the taxa names
#' @author Richel J.C. Bilderbeek
#' @export
get_taxa_names <- function(filename) {

  if (!file.exists(filename)) {
    stop(
      "'filename' must be the name of a file that is present. ",
      "File '", filename, "' not found"
    )
  }
  names(seqinr::read.fasta(filename))
}
