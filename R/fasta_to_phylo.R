#' Create a random phylogeny, with a specified crown age, from a FASTA file
#' @param fasta_filename Name of an existing FASTA file
#' @param crown_age the crown age of the phylogeny
#' @return a phylogeny
#' @examples
#'   fasta_filename <- beastscriptr::get_input_fasta_filename()
#'   phylo <- beastscriptr::fasta_to_phylo(fasta_filename, crown_age = 15)
#'   testit::assert(5 == length(phylo$tip.label))
#' @export
fasta_to_phylo <- function(fasta_filename, crown_age) {

  if (!file.exists(fasta_filename)) {
    stop("fasta_filename not found")
  }
  if (crown_age <= 0.0) {
    stop("crown_age must be nonzero and positive")
  }

  # Read the file
  sequences_dnabin <- ape::read.FASTA(fasta_filename)
  testit::assert(class(sequences_dnabin) == "DNAbin")

  # Extract the taxa names
  taxa_names <- names(sequences_dnabin)

  # Create a random tree ...
  phylo <- ape::rcoal(n = length(taxa_names), tip.label = taxa_names)

  # ... with the correct crown age
  phylo <- geiger::rescale(phylo, "depth", crown_age)

  phylo
}
