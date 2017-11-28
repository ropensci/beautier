context("create_beast2_input_state_tree")

test_that("two alignments with two initial trees", {


  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(fasta_filenames)
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 10)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 5)
  initial_phylogenies <- c(phylo1, phylo2)
  tree_priors <- beautier:::init_tree_priors(
    list(
      create_yule_tree_prior(),
      create_yule_tree_prior()
    ),
    ids = ids
  )
  testthat::expect_silent(
    beautier:::create_beast2_input_state_tree(
      tree_priors = tree_priors,
      initial_phylogenies = initial_phylogenies
    )
  )

})

test_that("abuse: two files, one phylogeny", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(fasta_filenames)
  phylos <- c(fasta_to_phylo(fasta_filename_1, crown_age = 10))
  tree_priors <- beautier:::init_tree_priors(
    list(create_yule_tree_prior(), create_yule_tree_prior()),
    ids = ids
  )
  testthat::expect_error(
    beautier:::create_beast2_input_state_tree(
      ids = c("Anthus_nd2", "Anthus_aco"),
      tree_priors = tree_priors,
      initial_phylogenies = pylos
    )
  )

})
