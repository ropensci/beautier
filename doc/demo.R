## ------------------------------------------------------------------------
library(beautier)

## ------------------------------------------------------------------------
fasta_filename <- get_beautier_path("test_output_0.fas")

## ------------------------------------------------------------------------
image(ape::read.FASTA(fasta_filename))

## ------------------------------------------------------------------------
# The name of the file you intend to let BEAST2 run
output_filename <- tempfile(pattern = "beeast2", fileext = ".xml")

create_beast2_input_file(
  fasta_filename,
  output_filename
)

## ------------------------------------------------------------------------
cat(readLines(output_filename), quote = FALSE, sep = '\n')

