#' Create a random phylogeny, with the same taxa names as the FASTA file
#'   and the desired crown age
#' @inheritParams default_params_doc
#' @return a a random phylogy, with the same taxa names as the FASTA file
#'   and the desired crown age
#' @examples
#'   # Create two random phylogies, with
#'   # - the same taxa names as the FASTA files
#'   # - the desired crown age
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   initial_phylogenies <- fastas_to_phylos(
#'     fasta_filenames,
#'     crown_age = 15
#'    )
#'
#'   # Crown age fixed to the crown age of the phylogeny
#'   create_beast2_input_file_1_12(
#'     input_filenames = fasta_filenames,
#'     "fastas_to_phylos.xml",
#'     fixed_crown_ages = c(TRUE, TRUE),
#'     initial_phylogenies = initial_phylogenies
#'   )
#'   testthat::expect_true(file.exists("fastas_to_phylos.xml"))
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
    phylos[[i]] <- fasta_to_phylo(fasta_filename, crown_age)
  }
  testit::assert(length(phylos) == length(fasta_filenames))
  phylos
}
