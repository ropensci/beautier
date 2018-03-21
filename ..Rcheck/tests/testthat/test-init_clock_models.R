context("init_clock_models")

test_that("initialize RLN clock model", {


  fasta_filenames <- beautier::get_beautier_path("test_output_0.fas")
  before <- list(create_rln_clock_model())
  testit::assert(is_rln_clock_model(before[[1]]))
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(
    before, fasta_filenames = fasta_filenames)
  testthat::expect_true(is_rln_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))

})

test_that("initialize strict clock model", {

  fasta_filenames <- beautier::get_beautier_path("test_output_0.fas")
  before <- list(create_strict_clock_model())
  testit::assert(is_strict_clock_model(before[[1]]))
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(
    before, fasta_filenames = fasta_filenames)
  testthat::expect_true(is_strict_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))
})

test_that("initialize RLN clock model", {

  fasta_filenames <- beautier::get_beautier_path("test_output_0.fas")
  clock_model <- create_rln_clock_model(
    ucldstdev_distr = create_gamma_distr(
      id = 0,
      alpha = create_alpha_param(id = 2, value = "0.5396"),
      beta = create_beta_param(id = 3, value = "0.3819")
    ),
    mparam_id = 1
  )
  before <- list(clock_model)
  testit::assert(is_rln_clock_model(before[[1]]))
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(
    before, fasta_filenames = fasta_filenames)
  testthat::expect_true(is_rln_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))
})

test_that("initialize RLN clock model with correct dimensions", {

  fasta_filenames <- beautier::get_beautier_path("test_output_5.fas")
  after <- beautier:::init_clock_models(
    list(create_rln_clock_model()), fasta_filenames = fasta_filenames)
  testthat::expect_equal(after[[1]]$dimension, 8)

  fasta_filenames <- beautier::get_beautier_path("test_output_6.fas")
  after <- beautier:::init_clock_models(
    list(create_rln_clock_model()), fasta_filenames = fasta_filenames)
  testthat::expect_equal(after[[1]]$dimension, 10)
})

test_that("initialize RLN clock model with correct mparam_id", {

  fasta_filenames <- beautier::get_beautier_path("test_output_5.fas")
  after <- beautier:::init_clock_models(
    list(create_rln_clock_model(mparam_id = 42)),
    fasta_filenames = fasta_filenames)
  testthat::expect_equal(after[[1]]$mparam_id, 42)

})
