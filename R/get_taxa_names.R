#' Extract the names of taxa from a file
#' @param filename name of a FASTA file
#' @return the taxa names
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   created <- get_taxa_names(get_beautier_path("anthus_aco_sub.fas"))
#'   expected <- c(
#'     "61430_aco", "626029_aco", "630116_aco", "630210_aco", "B25702_aco"
#'    )
#'   testit::assert(created == expected)
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
