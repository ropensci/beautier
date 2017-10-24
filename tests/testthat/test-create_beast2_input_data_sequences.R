context("create_beast2_input_data_sequences")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_data_sequences(
      get_input_fasta_filename()
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_data_sequences("nonse.nse")
  )
})
