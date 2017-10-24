context("helper")

test_that("locate test_output_0.fas", {
  filename <- get_input_fasta_filename()
  expect_equal(file.exists(filename), TRUE)
  expect_equal(file.exists(get_input_fasta_filename()), TRUE)
})

test_that("locate birth_death_2_4.xml", {
  filename <- system.file(
    "extdata", "birth_death_2_4.xml", package = "beastscriptr"
  )
  testthat::expect_true(file.exists(filename))
})
