#' Create one or more random phylogenies.
#'
#' Per FASTA file, one random phylogeny is created,
#' with the same taxa names as that FASTA file.
#' All phylogenies have the same crown age.
#' @inheritParams default_params_doc
#' @return a \code{multiPhylo} with as much phylogenies
#'   as there were FASTA filenames. Each phylogeny has
#'   the same taxa names as its corresponding FASTA file.
#'   All phylogenies have the same crown age.
#' @examples
#'   # Create two random phylogies, with
#'   # - the same taxa names as the FASTA files
#'   # - the desired crown age
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas")
#'   )
#'   initial_phylogenies <- fastas_to_phylos(
#'     fasta_filenames,
#'     crown_age = 15
#'   )
#' @author Richèl J.C. Bilderbeek
#' @export
fastas_to_phylos <- function(fasta_filenames, crown_age) {

  if (!files_exist(fasta_filenames)) {
    stop("'fasta_filenames' must be the names of existing files")
  }
  if (crown_age <= 0.0) {
    stop("'crown_age' must be nonzero and positive")
  }
  phylos <- list()
  for (i in seq_along(fasta_filenames)) {
    fasta_filename <- fasta_filenames[i]
    phylos[[i]] <- fasta_to_phylo(fasta_filename, crown_age) # nolint beautier function
  }
  testit::assert(length(phylos) == length(fasta_filenames))
  phylos
}
