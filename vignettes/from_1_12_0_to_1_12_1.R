## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loading_beautier----------------------------------------------------
library(beautier)

## ----is_beast2_input_file_safe-------------------------------------------
is_beast2_input_file_safe <- function(beast2_input_filename) {
  tryCatch(
    return(beautier:::is_beast2_input_file(beast2_input_filename)),
    error = function(cond) {
      print("BEAST2 not found")
    }
  )
  TRUE
}

## ----fix-----------------------------------------------------------------
fasta_filename <- get_fasta_filename()

create_beast2_input_file_1_12(
  input_filenames = fasta_filename,
  output_filename = "fix.xml",
  fixed_crown_ages = TRUE,
  initial_phylogenies = fasta_to_phylo(
    fasta_filename = fasta_filename,
    crown_age = 15
  )
)
testit::assert(is_beast2_input_file_safe("fix.xml"))

## ----fix_fix-------------------------------------------------------------
fasta_filenames <- get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))

create_beast2_input_file_1_12(
  input_filenames = fasta_filenames,
  output_filename = "fix_fix.xml",
  fixed_crown_ages = c(TRUE, TRUE),
  initial_phylogenies = fastas_to_phylos(
    fasta_filename = fasta_filenames,
    crown_age = 15
  )
)
testit::assert(is_beast2_input_file_safe("fix_fix.xml"))

## ----combinations--------------------------------------------------------
combinations <- list()
combinations[[1]] <- c(FALSE, FALSE)
combinations[[2]] <- c(TRUE, FALSE)
combinations[[3]] <- c(FALSE, TRUE)

for (i in seq_along(combinations)) {
  combination <- combinations[[i]]
  output_filename <- paste0("combination_", i, ".xml")
  
  create_beast2_input_file_1_12(
    input_filenames = fasta_filenames,
    output_filename = output_filename,
    fixed_crown_ages = combination,
    initial_phylogenies = fastas_to_phylos(
      fasta_filename = fasta_filenames,
      crown_age = 15
    )
  )
  testit::assert(is_beast2_input_file_safe(output_filename))
}

## ----12_34---------------------------------------------------------------
fasta_filenames <- get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))

initial_phylogenies <- list()
initial_phylogenies[[1]] <- fasta_to_phylo(fasta_filenames[1], crown_age = 12)
initial_phylogenies[[2]] <- fasta_to_phylo(fasta_filenames[2], crown_age = 34)

create_beast2_input_file_1_12(
  input_filenames = fasta_filenames,
  output_filename = "12_34.xml",
  fixed_crown_ages = c(TRUE, TRUE),
  initial_phylogenies = initial_phylogenies
)

testit::assert(is_beast2_input_file_safe("12_34.xml"))

## ----cleanup, include = FALSE--------------------------------------------
# Cleaning up
filenames <- c(
  "fix.xml", 
  "fix_fix.xml", 
  paste0("combination_", seq(1, 3), ".xml"), 
  "12_34.xml"
)

for (filename in filenames) {
  if (file.exists(filename)) file.remove(filename)
}

