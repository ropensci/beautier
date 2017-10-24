#' Get the path of a fasta file used in testing
#' @return the path of a fasta file used in testing
#' @examples
#'   my_file <- beastscriptr::get_input_fasta_filename()
#'   does_exist <- file.exists(my_file)
#'   testit::assert(does_exist == TRUE)
#' @export
get_input_fasta_filename <- function() {

  return(
    system.file(
      "extdata", "test_output_0.fas", package = "beastscriptr"
    )
  )
}
