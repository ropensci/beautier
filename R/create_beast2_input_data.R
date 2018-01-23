#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_data <- function(
  input_filenames,
  misc_options = create_misc_options()
) {
  testit::assert(files_exist(input_filenames)) # nolint internal function

  text <- NULL
  n <- length(input_filenames)
  for (i in seq(1, n)) {
    input_fasta_filename <- input_filenames[i]
    id <- beautier::get_id(
      input_fasta_filename,
      capitalize_first_char_id = misc_options$capitalize_first_char_id
    )
    if (i == 1) {
      text <- c(text, "    <data")
    } else {
      text <- c(text, "<data")
    }
    text <- c(text, paste0("id=\"", id, "\""))
    text <- c(text, "name=\"alignment\">")
    text <- c(
      text,
      create_beast2_input_data_sequences(
        input_fasta_filename = input_fasta_filename,
        nucleotides_uppercase = misc_options$nucleotides_uppercase
      )
    )
    text <- c(text, indent("</data>", n_spaces = 16)) # nolint internal function
  }
  text
}
