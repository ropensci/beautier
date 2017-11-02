context("create_beast2_input_state_tree")

test_that("two alignments with two initial trees", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 10)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 5)
  initial_phylogenies <- c(phylo1, phylo2)
  testthat::expect_silent(
    create_beast2_input_state_tree(
      ids = c("Anthus_nd2", "Anthus_aco"),
      initial_phylogenies = initial_phylogenies
    )
  )

})

test_that("abuse: two files, one phylogeny", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  phylos <- c(fasta_to_phylo(fasta_filename_1, crown_age = 10))
  testthat::expect_error(
    create_beast2_input_state_tree(
      ids = c("Anthus_nd2", "Anthus_aco"),
      initial_phylogenies = pylos
    )
  )

})

