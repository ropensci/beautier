#' Removes files if and only if present
#' @param filenames names of the files to be removed
#' @return nothing, removes those files that are present
#' @examples
#'   # Can safely delete absent files
#'   beautier:::remove_files("abs.ent")
#'
#'   # Create a file
#'   fasta_filename <- "remove_files.fas"
#'   beastier:::create_random_fasta(
#'     n_taxa = 5,
#'     sequence_length = 10,
#'     fasta_filename = fasta_filename
#'   )
#'   testit::assert(file.exists(fasta_filename))
#'
#'   # Can safely delete a mix of present and absent files
#'   beautier:::remove_files(c("abs.ent", fasta_filename))
#'   testit::assert(!file.exists(fasta_filename))
#' @author Richel J.C. Bilderbeek
remove_files <- function(filenames) {
  for (filename in filenames) {
    if (file.exists(filename)) file.remove(filename)
  }
}
