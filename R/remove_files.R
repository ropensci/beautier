#' Removes files if and only if present
#' @param filenames names of the files to be removed
#' @return nothing, removes those files that are present
#' @examples
#'   # Can safely delete absent files
#'   beautier:::remove_files("abs.ent")
#'
#'   # Create a file
#'   filename <- "remove_files.fas"
#'   beautier:::create_random_fasta(
#'     n_taxa = 5,
#'     sequence_length = 10,
#'     fasta_filename = fasta_filename
#'   )
#'   testit::assert(file.exists(filename))
#'
#'   # Can safely delete a mix of present and absent files
#'   beautier:::remove_files(c("abs.ent", filename))
#'   testit::assert(!file.exists(filename))
#' @author Richel J.C. Bilderbeek
remove_files <- function(filenames) {
  for (filename in filenames) {
    if (file.exist(filename)) file.remove(filename)
  }
}
