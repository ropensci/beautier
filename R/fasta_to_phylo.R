#' Create a random phylogeny, with a specified crown age, from a FASTA file
#' @inheritParams default_params_doc
#' @return a phylogeny
#' @examples
#'   # Create a random phylogy, with
#'   # - the same taxa names as the FASTA
#'   # - the desired crown age
#'   fasta_filename <- get_fasta_filename()
#'   initial_phylogeny <- fasta_to_phylo(
#'     fasta_filename,
#'     crown_age = 15
#'    )
#'
#'   # Crown age fixed to the crown age of the phylogeny
#'   create_beast2_input_file(
#'     input_fasta_filenames = fasta_filename,
#'     "fasta_to_phylo.xml",
#'     fixed_crown_age = TRUE,
#'     initial_phylogenies = initial_phylogeny
#'   )
#'   testthat::expect_true(file.exists("fasta_to_phylo.xml"))
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
