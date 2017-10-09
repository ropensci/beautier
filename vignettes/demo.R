## ------------------------------------------------------------------------
fasta_filename <- tempfile(pattern = "demo", fileext = ".fas")

beastscriptr::create_random_fasta(
  n_taxa = 5, # species
  sequence_length = 20, # DNA nucleotides
  filename = fasta_filename
)

## ------------------------------------------------------------------------
image(ape::read.FASTA(fasta_filename))

## ------------------------------------------------------------------------
# The name of the file you intend to let BEAST2 run
output_xml_filename <- tempfile(pattern = "demo", fileext = ".xml")

beastscriptr::beast_scriptr(
  input_fasta_filename = fasta_filename,
  mcmc_chainlength = 10000000,
  tree_prior = "birth_death",
  output_xml_filename = output_xml_filename
)

## ------------------------------------------------------------------------
print(readLines(output_xml_filename))

