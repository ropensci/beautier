## ------------------------------------------------------------------------
library(beautier)

## ------------------------------------------------------------------------
fasta_filename <- get_path("test_output_0.fas")

## ------------------------------------------------------------------------
image(ape::read.FASTA(fasta_filename))

## ------------------------------------------------------------------------
# The name of the file you intend to let BEAST2 run
output_xml_filename <- "beast2.xml"

create_beast2_input_file(
  fasta_filename,
  output_xml_filename
)

## ------------------------------------------------------------------------
cat(readLines(output_xml_filename), quote = FALSE, sep = '\n')

## ----cleanup, include = FALSE--------------------------------------------
# Cleaning up
filenames <- c(
  "beast2.xml"
)
for (filename in filenames) {
  if (file.exists(filename)) file.remove(filename)
}

