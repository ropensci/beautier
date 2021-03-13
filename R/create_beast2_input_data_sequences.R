#' Creates the data section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @param input_fasta_filename one FASTA filename
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_data_sequences <- function( # nolint indeed long function name
  input_fasta_filename,
  beauti_options = beautier::create_beauti_options()
) {
  beautier::check_file_exists(input_fasta_filename, "input_fasta_filename")

  nucleotides_uppercase <- beauti_options$nucleotides_uppercase

  sequences_table <- beautier::fasta_file_to_sequences(input_fasta_filename)
  sequences <- as.matrix(cbind(rownames(sequences_table), sequences_table))
  text <- NULL
  for (i in seq(1, nrow(sequences))) {
    row <- sequences[i, ]
    nextline <- paste0(
      "<sequence id=\"seq_",
      trimws(row[1]),
      "\" "
    )
    if (beauti_options$beast2_version == "2.6") {
      nextline <- paste0(nextline, "spec=\"Sequence\" ")
    }
    nextline <- paste0(nextline,
      "taxon=\"",
      trimws(row[1]),
      "\" totalcount=\"4\" value=\"",
      ifelse(nucleotides_uppercase == TRUE, toupper(row[2]), row[2]),
      "\"/>" # nolint this is no absolute path
    )
    text <- c(text, nextline)
  }
  text <- sort(text, method = "radix")

  if (beauti_options$beast2_version == "2.6") {
    text <- beautier::interspace(text)
    text <- stringr::str_replace_all(
      string = text,
      pattern = "$^",
      replacement = paste0(
        rep(" ", 20),
        collapse = ""
      )
    )
  }

  beautier::indent(text = text, n_spaces = beauti_options$sequence_indent)
}
