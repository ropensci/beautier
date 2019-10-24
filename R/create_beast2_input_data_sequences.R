#' Creates the data section of a BEAST2 XML parameter file
#' @param input_fasta_filename one FASTA filename
#' @param nucleotides_uppercase are the nucleotides written in uppercase?
#'   Yes if TRUE, no if FALSE
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_data_sequences <- function( # nolint beautier function
  input_fasta_filename,
  beauti_options = create_beauti_options()
) {
  beautier::check_file_exists(input_fasta_filename, "input_fasta_filename")

  nucleotides_uppercase <- beauti_options$nucleotides_uppercase

  sequences_table <- fasta_file_to_sequences(
    input_fasta_filename)
  sequences <- as.matrix(cbind(rownames(sequences_table), sequences_table))
  text <- NULL
  for (i in seq(1, nrow(sequences))) {
    row <- sequences[i, ]
    nextline <- paste0(
      "<sequence id=\"seq_",
      trimws(row[1]),
      "\" taxon=\"",
      trimws(row[1]),
      "\" totalcount=\"4\" value=\"",
      ifelse(nucleotides_uppercase == TRUE, toupper(row[2]), row[2]),
      "\"/>" # nolint this is no absolute path
    )
    text <- c(text, nextline)
  }
  text <- sort(text, method = "radix")
  beautier::indent(text = text, n_spaces = beauti_options$sequence_indent)
}
