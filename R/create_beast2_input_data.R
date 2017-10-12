#' Creates the data section of a BEAST2 XML parameter file
#' @param input_fasta_filenames name of the FASTA file
#' @export
create_beast2_input_data <- function(
  input_fasta_filenames
) {
  if (!file.exists(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", get_file_base_sans_ext(input_fasta_filenames), "\""))
  text <- c(text, "name=\"alignment\">")
  text <- c(text, create_beast2_input_data_sequences(input_fasta_filenames = input_fasta_filenames))
  text <- c(text, "                </data>")
  text
}

#' Creates the data section of a BEAST2 XML parameter file
#' @param input_fasta_filenames name of the FASTA file
#' @export
create_beast2_input_data_sequences <- function(
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
