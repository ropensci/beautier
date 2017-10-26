#' Conclude the ID from a FASTA filename
#' @param fasta_filename name of a FASTA file
#' @return the ID
#' @examples
#'   testit::assert(get_id("anthus_aco.fas") == "Anthus_aco")
#' @author Richel J.C. Bilderbeek
#' @export
get_id <- function(fasta_filename) {
  file_base <- beastscriptr::get_file_base_sans_ext(fasta_filename)
  id <- paste0(toupper(substr(file_base, 1, 1)), substring(file_base, 2))
  id
}
