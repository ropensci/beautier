context("is_beast2_input_file")

test_that("FASTA file is not a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  filename <- beautier::get_input_fasta_filename()
  is_ok <- NULL

  testthat::expect_output(
    is_ok <- is_beast2_input_file(filename, verbose = TRUE)
  )

  testthat::expect_false(is_ok)

})

test_that("invalid.xml is not a valid BEAST2 input file", {

  # Gives a status error
  if (!beautier::is_on_travis()) return()

  filename <- system.file(
    "extdata", "invalid.xml", package = "beautier"
  )

  is_ok <- NULL

  testthat::expect_output(
    is_ok <- is_beast2_input_file(filename, verbose = TRUE)
  )

  testthat::expect_false(is_ok)

})

test_that("birth_death_2_4.xml is valid", {

  if (!beautier::is_on_travis()) return()

  filename <- system.file(
    "extdata", "birth_death_2_4.xml", package = "beautier"
  )
  testthat::expect_true(file.exists(filename))
  testthat::expect_true(is_beast2_input_file(filename))

})

test_that("anthus_2_4.xml is valid", {

  if (!beautier::is_on_travis()) return()

  filename <- system.file(
    "extdata", "anthus_2_4.xml", package = "beautier"
  )
  testthat::expect_true(file.exists(filename))
  testthat::expect_true(is_beast2_input_file(filename))

})

test_that("abuse", {

  if (!beautier::is_on_travis()) return()

  testthat::expect_error(
    is_beast2_input_file("abs.ent")
  )

  testthat::expect_error(
    is_valid_beast2_input_file("abs.ent")
  )

})
