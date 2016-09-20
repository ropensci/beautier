## ------------------------------------------------------------------------
library(beastscriptr)

## ------------------------------------------------------------------------
n_taxa <- 5
dna_sequence_length_nucleotides <- 20
mutation_rate <- 0.1

alignment <- create_random_alignment(
  n_taxa = n_taxa,
  sequence_length = dna_sequence_length_nucleotides,
  rate = mutation_rate
)

image(alignment)

## ------------------------------------------------------------------------
fasta_filename <- "demo.fas"

phangorn::write.phyDat(
  x = alignment, 
  file = fasta_filename, 
  format = "fasta"
)
if (file.exists(fasta_filename)) { 
  print("File created") 
} else {
  print("Error: file not created") 
}

## ------------------------------------------------------------------------
# The MCMC chain length BEAST2 will have to use
mcmc_chainlength <- 10000000

# The tree prior BEAST2 will have to use, can be 'birth_death' or 'coalescent_constant_population'
tree_prior <- "birth_death"
# tree_prior <- "coalescent_constant_population"

# The name of the file you intend to let BEAST2 run
output_xml_filename <- "demo.xml"

## ------------------------------------------------------------------------

beast_scriptr(
  input_fasta_filename = fasta_filename,
  mcmc_chainlength = mcmc_chainlength,
  tree_prior = tree_prior,
  output_xml_filename = output_xml_filename
)

if (file.exists(output_xml_filename)) { 
  print("File created") 
} else {
  print("Error: file not created") 
}

## ------------------------------------------------------------------------
print(readLines(output_xml_filename))

## ------------------------------------------------------------------------
file.remove(fasta_filename)
file.remove(output_xml_filename)

