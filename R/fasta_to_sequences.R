#' Convert a FASTA file to a table of sequences
#' @param fasta_filename Name of an existing FASTA file
#' @return a table of sequences
#' @export
fasta_file_to_sequences <- function(fasta_filename) {

  if (!file.exists(fasta_filename)) {
    stop("fasta_filename not found")
  }

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
