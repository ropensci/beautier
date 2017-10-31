context("is_beast2_input_file")

test_that("testing FASTA file is not a valid BEAST2 input file", {

  filename <- beastscriptr::get_input_fasta_filename()
  testthat::expect_false(
    is_beast2_input_file(filename)
  )

})

test_that("birth_death_2_4.xml is valid", {

  filename <- system.file(
    "extdata", "birth_death_2_4.xml", package = "beastscriptr"
  )
  testthat::expect_true(file.exists(filename))
  testthat::expect_true(is_beast2_input_file(filename))

})

test_that("anthus_2_4.xml is valid", {

  filename <- system.file(
    "extdata", "anthus_2_4.xml", package = "beastscriptr"
  )
  testthat::expect_true(file.exists(filename))
  testthat::expect_true(is_beast2_input_file(filename))

})

test_that("abuse", {

  testthat::expect_error(
    is_beast2_input_file("abs.ent")
  )

  testthat::expect_error(
    is_valid_beast2_input_file("abs.ent")
  )

})
