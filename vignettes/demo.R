## ------------------------------------------------------------------------
alignment <- beastscriptr::create_random_alignment(
  n_taxa = 5,
  sequence_length = 20,
  rate = 0.1
)

image(alignment)

## ------------------------------------------------------------------------
n_taxa <- 5
sequence_length <- 10
filename <- "create_random_fasta.fasta"
beastscriptr::create_random_fasta(
  n_taxa,
  sequence_length,
  filename
)
file.show(filename)

