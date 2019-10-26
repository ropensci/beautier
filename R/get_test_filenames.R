#' Get the path of a FASTA file used in testing
#' @return the path of a FASTA file used in testing
#' @examples
#'   filename <- beautier::get_fasta_filename()
#'   testit::assert(file.exists(filename))
#'
#'   create_beast2_input_file(
#'     input_filename = filename,
#'     output_filename = tempfile(pattern = "beast", fileext = ".xml")
#'   )
#' @author Richèl J.C. Bilderbeek
#' @export
get_fasta_filename <- function() {
  beautier::get_beautier_path("test_output_0.fas") # nolint beautier function
}
