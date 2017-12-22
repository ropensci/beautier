#' Creates the data section of a BEAST2 XML parameter file
#' @param input_fasta_filename one FASTA filename
#' @param nucleotides_uppercase are the nucleotides written in uppercase?
#'   Yes if TRUE, no if FALSE
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_data_sequences <- function( # nolint internal function
  input_fasta_filename,
  nucleotides_uppercase = FALSE
) {
  testit::assert(file.exists(input_fasta_filename))

  sequences_table <- fasta_file_to_sequences(
    input_fasta_filename)
  sequences <- as.matrix(cbind(rownames(sequences_table), sequences_table))
  text <- NULL
  for (i in seq(1, nrow(sequences))) {
    row <- sequences[i, ]
    nextline <- paste0(
      "                    <sequence id=\"seq_",
      trimws(row[1]),
      "\" taxon=\"",
      trimws(row[1]),
      "\" totalcount=\"4\" value=\"",
      ifelse(nucleotides_uppercase == TRUE, toupper(row[2]), row[2]),
      "\"/>"
    )
    text <- c(text, nextline)
  }
  text <- sort(text, method = "radix")
  text
}
