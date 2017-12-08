#' Get the path of a FASTA file used in testing
#' @return the path of a fasta file used in testing
#' @examples
#'   filename <- beautier::get_fasta_filename()
#'   testit::assert(file.exists(filename))
#'
#'   create_beast2_input_file(
#'     input_fasta_filenames = filename,
#'     "my_beast.xml"
#'   )
#' @export
get_fasta_filename <- function() {
  get_path("test_output_0.fas")
}
