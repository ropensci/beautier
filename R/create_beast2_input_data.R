#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @noRd
create_beast2_input_data <- function(
  input_filenames,
  misc_options = create_misc_options()
) {
  testit::assert(files_exist(input_filenames)) # nolint internal function

  text <- NULL
  n <- length(input_filenames)
  for (i in seq(1, n)) {
    input_fasta_filename <- input_filenames[i]
    id <- beautier::get_alignment_id(
      input_fasta_filename,
      capitalize_first_char_id = misc_options$capitalize_first_char_id
    )
    text <- c(text, create_data_xml(
      id = id,
      beast2_version = misc_options$beast2_version)
    )
    text <- c(
      text,
      create_beast2_input_data_sequences(
        input_fasta_filename = input_fasta_filename,
        misc_options = misc_options
      )
    )
    text <- c(text, indent("</data>", n_spaces = 16)) # nolint internal function
  }
  text
}
