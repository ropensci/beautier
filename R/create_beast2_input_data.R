#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_data <- function(
  input_fasta_filenames
) {
  if (!beastscriptr::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  # Alignment IDs
  ids <- beastscriptr::get_file_base_sans_ext(input_fasta_filenames)

  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", ids, "\""))
  text <- c(text, "name=\"alignment\">")
  text <- c(text, beastscriptr::create_beast2_input_data_sequences(
    input_fasta_filenames = input_fasta_filenames))
  text <- c(text, "                </data>")
  text
}
