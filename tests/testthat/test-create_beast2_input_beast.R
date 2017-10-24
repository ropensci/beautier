context("create_beast2_input_beast")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_beast(
      input_fasta_filenames = get_input_fasta_filename())
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_beast(input_fasta_filenames = "nonsense")
  )

})
