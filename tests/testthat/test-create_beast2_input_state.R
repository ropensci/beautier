context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    create_beast2_input_state(
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      tree_priors = create_tree_prior("birth_death"),
      initial_phylogeny = NA
    )
  )
})

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_state(
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      tree_priors = create_tree_prior("coalescent_constant_population"),
      initial_phylogeny = NA
    )
  )
})
