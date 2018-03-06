context("helper")

test_that("locate test_output_0.fas", {
  filename <- get_fasta_filename()
  expect_equal(file.exists(filename), TRUE)
  expect_equal(file.exists(get_fasta_filename()), TRUE)
})

test_that("locate bdh_2_4.xml", {
  filename <- beautier::get_beautier_path("bd_2_4.xml")
  testthat::expect_true(file.exists(filename))
})
