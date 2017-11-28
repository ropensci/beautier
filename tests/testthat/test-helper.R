context("helper")

test_that("locate test_output_0.fas", {
  filename <- get_input_fasta_filename()
  expect_equal(file.exists(filename), TRUE)
  expect_equal(file.exists(get_input_fasta_filename()), TRUE)
})

test_that("locate bdh_2_4.xml", {
  filename <- system.file(
    "extdata", "bd_2_4.xml", package = "beautier"
  )
  testthat::expect_true(file.exists(filename))
})
