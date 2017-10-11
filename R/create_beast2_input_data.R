#' Creates the data section of a BEAST2 XML parameter file
#' @param filename_base base of the filenames
#' @param input_fasta_filenames name of the FASTA file
#' @export
create_beast2_input_data <- function(
  filename_base,
  input_fasta_filenames
) {
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", filename_base, "\""))
  text <- c(text, "name=\"alignment\">")
  sequences_table <- beastscriptr::fasta_file_to_sequences(
    input_fasta_filenames)
  sequences <- cbind(rownames(sequences_table), sequences_table)

  apply(sequences, 1, function(row) {
      nextline <- paste0(
        "                    <sequence id=\"seq_",
        row[1],
        "\" taxon=\"",
        row[1],
        "\" totalcount=\"4\" value=\"",
        toupper(row[2]),
        "\"/>",
        sep = ""
      )
      text <<- c(text, nextline)
    }
  )
  text <- c(text, "                </data>")
  text
}
