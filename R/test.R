#' Get the path of a fasta file used in testing
#' @return the path of a fasta file used in testing
#' @examples
#'   my_file <- beastscriptr::get_input_fasta_filename()
#'   does_exist <- file.exists(my_file)
#'   testit::assert(does_exist)
#' @export
get_input_fasta_filename <- function()
{
  my_file <- paste(getwd(), "/../../inst/extdata/test_output_0.fas", sep = "")
  if (!file.exists(my_file))
  {
    stop("get_input_fasta_filename: file not found '", my_file, "'")
  }
  my_file
}

#' Get the path of a BEAST2 XML parameter file used in testing
#' @return the path of a BEAST2 XML parameter file used in testing
#' @examples
#'   my_file <- beastscriptr::get_output_xml_filename()
#'   does_exist <- file.exists(my_file)
#'   testit::assert(does_exist)
#' @export
get_output_xml_filename <- function()
{
  my_file <- paste(getwd(), "/../../inst/extdata/birth_death_0_20151005.xml", sep = "")
  if (!file.exists(my_file))
  {
    stop("get_input_fasta_filename: file not found '", my_file, "'")
  }
  my_file
}
