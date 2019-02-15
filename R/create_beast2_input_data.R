#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_data <- function(
  input_filenames,
  beauti_options = create_beauti_options()
) {
  testit::assert(files_exist(input_filenames)) # nolint beautier function

  text <- NULL
  n <- length(input_filenames)
  for (i in seq(1, n)) {
    input_fasta_filename <- input_filenames[i]
    id <- beautier::get_alignment_id(
      input_fasta_filename,
      capitalize_first_char_id = beauti_options$capitalize_first_char_id
    )
    text <- c(text, create_data_xml(
      id = id,
      beast2_version = beauti_options$beast2_version)
    )
    text <- c(
      text,
      create_beast2_input_data_sequences(
        input_fasta_filename = input_fasta_filename,
        beauti_options = beauti_options
      )
    )
    testit::assert(beauti_options$sequence_indent >= 20)
    text <- c(text, indent("</data>", n_spaces = beauti_options$sequence_indent - 4)) # nolint beautier function
  }
  text
}
