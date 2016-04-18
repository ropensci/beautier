## ------------------------------------------------------------------------
library(beastscriptr)

## ------------------------------------------------------------------------
alignment <- create_random_alignment(
  n_taxa = 5,
  sequence_length = 20,
  rate = 0.1
)

image(alignment)

## ------------------------------------------------------------------------
n_taxa <- 5
sequence_length <- 10
#filename <- paste(getwd(), "/create_random_fasta.fasta", sep = "")
filename <- "create_random_fasta.fasta"
create_random_fasta(
  n_taxa = n_taxa,
  sequence_length = sequence_length,
  filename = filename
)
readLines(filename)

