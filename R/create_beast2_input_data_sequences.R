#' Creates the data section of a BEAST2 XML parameter file
#' @param input_fasta_filenames name of the FASTA file
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @export
create_beast2_input_data_sequences <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  input_fasta_filenames
) {
  if (!file.exists(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  sequences_table <- beastscriptr::fasta_file_to_sequences(
    input_fasta_filenames)
  sequences <- as.matrix(cbind(rownames(sequences_table), sequences_table))
  text <- NULL
  for (i in seq(1, nrow(sequences))) {
    row <- sequences[i, ]
    nextline <- paste0(
      "                    <sequence id=\"seq_",
      row[1],
      "\" taxon=\"",
      row[1],
      "\" totalcount=\"4\" value=\"",
      row[2],
      "\"/>"
    )
    text <- c(text, nextline)
  }
  text <- sort(text)
  text
}
