## ------------------------------------------------------------------------
library(beautier)

## ------------------------------------------------------------------------
fasta_filename <- get_path("test_output_0.fas")

## ------------------------------------------------------------------------
image(ape::read.FASTA(fasta_filename))

## ------------------------------------------------------------------------
# The name of the file you intend to let BEAST2 run
output_xml_filename <- tempfile(pattern = "demo", fileext = ".xml")

beautier::create_beast2_input_file(
  fasta_filename,
  output_xml_filename
)

## ------------------------------------------------------------------------
cat(readLines(output_xml_filename), quote = FALSE, sep = '\n')

