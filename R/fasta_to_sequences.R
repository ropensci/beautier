#' Convert a FASTA file to a table of sequences
#' @param fasta_filename One existing FASTA filenames
#' @return a table of sequences
#' @examples
#' check_empty_beautier_folder()
#'
#' fasta_file_to_sequences(fasta_filename = get_fasta_filename())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
fasta_file_to_sequences <- function(fasta_filename) {
  beautier::check_file_exists(fasta_filename, "fasta_filename")

  # Read the file
  sequences_dnabin <- ape::read.FASTA(fasta_filename)
  testit::assert(class(sequences_dnabin) == "DNAbin")

  # Convert the file to a table with labels and sequences
  labels <- names(sequences_dnabin)
  chars <- as.character(sequences_dnabin)
  sequences <- NULL

  for (line in chars) {
    sequence <- utils::capture.output(cat(line, sep = ""))
    sequences <- c(sequences, sequence)
  }

  table <- data.frame(sequences, row.names = labels)
  return(table)
}
