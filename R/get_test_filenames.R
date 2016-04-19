#' Get the path of a fasta file used in testing
#' @return the path of a fasta file used in testing
#' @examples
#'   my_file <- beastscriptr::get_input_fasta_filename()
#'   does_exist <- file.exists(my_file)
#'   testit::assert(does_exist)
#' @export
get_input_fasta_filename <- function() {
  filenames <- c(
    "test_output_0.fas",
    "~/inst/extdata/test_output_0.fas",                                               # nolint
    "~/beastscriptr/inst/extdata/test_output_0.fas",                                  # nolint
    "~/GitHubs/beastscriptr/inst/extdata/test_output_0.fas",                          # nolint
    "/home/travis/build/richelbilderbeek/beastscriptr/inst/extdata/test_output_0.fas" # nolint
  )
  for (filename in filenames) {
    if (file.exists(filename)) {
      return(filename)
    }
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
get_output_xml_filename <- function() {
  filenames <- c(
    "birth_death_0_20151005.xml",
    "~/inst/extdata/birth_death_0_20151005.xml",                                                # nolint
    "~/beastscriptr/inst/extdata/birth_death_0_20151005.xml",                                   # nolint
    "~/GitHubs/beastscriptr/inst/extdata/birth_death_0_20151005.xml",                           # nolint
    "/home/travis/build/richelbilderbeek/beastscriptr/inst/extdata/birth_death_0_20151005.xml"  # nolint
  )
  for (filename in filenames) {
    if (file.exists(filename)) {
      return(filename)
    }
  }
  stop(
    "get_output_xml_filename: ",
    "cannot find the 'birth_death_0_20151005.xml' file"
  )
}
