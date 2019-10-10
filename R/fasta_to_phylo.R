#' Create a random phylogeny, with the same taxa names as the FASTA file
#'   and the desired crown age
#' @inheritParams default_params_doc
#' @return a a random phylogeny, with the same taxa names as the FASTA file
#'   and the desired crown age
#' @examples
#'   # Create a random phylogeny, with
#'   # - the same taxa names as the FASTA file
#'   # - the desired crown age
#'   fasta_filename <- get_fasta_filename()
#'   initial_phylogeny <- fasta_to_phylo(
#'     fasta_filename,
#'     crown_age = 15
#'   )
#' @author Richèl J.C. Bilderbeek
#' @export
fasta_to_phylo <- function(fasta_filename, crown_age) {
  beautier::check_file_exists(fasta_filename, "fasta_filename")
  if (crown_age <= 0.0) {
    stop("'crown_age' must be nonzero and positive")
  }

  # Read the file
  sequences_dnabin <- ape::read.FASTA(fasta_filename)
  testit::assert(class(sequences_dnabin) == "DNAbin")

  # Extract the taxa names
  taxa_names <- names(sequences_dnabin)

  # Create a random tree ...
  phylo <- ape::rcoal(n = length(taxa_names), tip.label = taxa_names)
  # ... with the correct crown age
  geiger::rescale(phylo, "depth", crown_age)
}
