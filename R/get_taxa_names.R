#' Extract the names of taxa from a file
#' @param filename name of a FASTA file
#' @return the taxa names
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' get_taxa_names(get_beautier_path("anthus_aco_sub.fas"))
#'
#' check_empty_beautier_folder()
#' @export
get_taxa_names <- function(filename) {
  beautier::check_file_exists(filename, "filename")
  names(seqinr::read.fasta(filename))
}
