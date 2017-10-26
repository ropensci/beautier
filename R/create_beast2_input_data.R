#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_data <- function(
  input_fasta_filenames,
  misc_options = create_misc_options()
) {
  if (!beastscriptr::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }

  text <- NULL
  for (input_fasta_filename in input_fasta_filenames) {
    id <- beastscriptr::get_id(
      input_fasta_filename,
      capitalize_first_char_id = misc_options$capitalize_first_char_id
    )
    text <- c(text, "    <data")
    text <- c(text, paste0("id=\"", id, "\""))
    text <- c(text, "name=\"alignment\">")
    text <- c(
      text,
      beastscriptr::create_beast2_input_data_sequences(
        input_fasta_filename = input_fasta_filename,
        nucleotides_uppercase = misc_options$nucleotides_uppercase
      )
    )
    text <- c(text, "                </data>")
  }
  text
}
