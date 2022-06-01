## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)
beautier::remove_beautier_folder()
beautier::check_empty_beautier_folder()

## ----load_beautier------------------------------------------------------------
library(beautier)

## ----get_fasta_filename-------------------------------------------------------
fasta_filename <- get_beautier_path("test_output_0.fas")

## ----show_alignment-----------------------------------------------------------
image(ape::read.FASTA(fasta_filename))

## ----create_output_filename---------------------------------------------------
output_filename <- get_beautier_tempfilename()
output_filename

## ----create_beast2_input_file-------------------------------------------------
create_beast2_input_file(
  fasta_filename,
  output_filename
)

## ----show_beast2_input_file---------------------------------------------------
readLines(output_filename)

## ----cleanup------------------------------------------------------------------
file.remove(output_filename)

beautier::remove_beautier_folder()
beautier::check_empty_beautier_folder()

