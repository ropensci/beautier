get_existing_filename <- function() {
  # Return an existing .trees filename

}



#' Get the path of a fasta file used in testing
#' @return the path of a fasta file used in testing
#' @examples
#'   my_file <- beastscriptr::get_input_fasta_filename()
#'   does_exist <- file.exists(my_file)
#'   testit::assert(does_exist)
#' @export
get_input_fasta_filename <- function()
{
  filenames <- c(
    "test_output_0.fas",
    "~/inst/extdata/test_output_0.fas",
    "~/beastscriptr/inst/extdata/test_output_0.fas",
    "~/GitHubs/beastscriptr/inst/extdata/test_output_0.fas"
  )
  for (filename in filenames) {
    if (file.exists(filename)) { return (filename) }
  }
  stop(
    "get_input_fasta_filename: ",
    "cannot find the 'test_output_0.fas' file"
  )
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
  filenames <- c(
    "birth_death_0_20151005.xml",
    "~/inst/extdata/birth_death_0_20151005.xml",
    "~/beastscriptr/inst/extdata/birth_death_0_20151005.xml",
    "~/GitHubs/beastscriptr/inst/extdata/birth_death_0_20151005.xml"
  )
  for (filename in filenames) {
    if (file.exists(filename)) { return (filename) }
  }
  stop(
    "get_output_xml_filename: ",
    "cannot find the 'birth_death_0_20151005.xml' file"
  )
}
