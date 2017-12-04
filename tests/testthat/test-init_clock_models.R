context("init_clock_models")

test_that("initialize RLN clock model", {

  id <- "a"
  before <- list(create_rln_clock_model())
  testit::assert(is_rln_clock_model(before[[1]]))
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(before, ids = id)
  testthat::expect_true(is_rln_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))

})

test_that("initialize strict clock model", {

  id <- "a"
  before <- list(create_strict_clock_model())
  testit::assert(is_strict_clock_model(before[[1]]))
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(before, ids = id)
  testthat::expect_true(is_strict_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))
})

test_that("initialize RLN clock model", {

  id <- "a"
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
  after <- beautier:::init_clock_models(before, ids = id)
  testthat::expect_true(is_rln_clock_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_clock_models(after))

})
