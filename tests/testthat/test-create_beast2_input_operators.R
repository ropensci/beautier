context("create_beast2_input_operators")

test_that("usage", {
  testthat::expect_silent(
    create_beast2_input_operators(
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      tree_priors = create_tree_prior("birth_death"),
      fixed_crown_age = FALSE
    )
  )
})
