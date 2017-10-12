context("create_beast2_input_distribution")

test_that("usage", {
  testthat::expect_silent(
    create_beast2_input_distribution(
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      tree_priors = create_tree_prior(name = "birth_death")
    )
  )
})
