context("create_beast2_input_loggers")

test_that("multiplication works", {
  testthat::expect_silent(
    create_beast2_input_loggers(
      fasta_filenames = get_input_fasta_filename(),
      tree_priors = create_tree_prior("birth_death")
    )
  )

})
