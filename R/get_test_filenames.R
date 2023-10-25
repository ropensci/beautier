#' Get the path of a FASTA file used in testing
#' @return the path of a FASTA file used in testing
#' @examples
#' if (is_on_ci()) {
#'
#'   input_filename <- get_fasta_filename()
#'   output_filename <- get_beautier_tempfilename()
#'
#'   create_beast2_input_file(
#'     input_filename = input_filename,
#'     output_filename = output_filename
#'   )
#'   file.remove(output_filename)
#'
#'   remove_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
get_fasta_filename <- function() {
  get_beautier_path("test_output_0.fas")
}
