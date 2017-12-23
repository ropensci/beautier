#' Removes files if and only if present
#' @param filenames names of the files to be removed
#' @return nothing, removes those files that are present
#' @examples
#'   # Can safely delete absent files
#'   beautier:::remove_files("abs.ent")
#'
#'   # Create a file
#'   filename <- "remove_files.xml"
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     filename
#'   )
#'   testit::assert(file.exists(filename))
#'
#'   # Can safely delete a mix of present and absent files
#'   beautier:::remove_files(c("abs.ent", filename))
#'   testit::assert(!file.exists(filename))
#' @author Richel J.C. Bilderbeek
remove_files <- function(filenames) {
  for (filename in filenames) {
    if (file.exists(filename)) file.remove(filename)
  }
}
