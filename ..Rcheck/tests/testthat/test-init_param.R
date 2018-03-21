context("init_param")

test_that("use", {

  alpha_param <- create_alpha_param()
  testthat::expect_false(beautier:::is_init_param(alpha_param))
  alpha_param <- beautier:::init_param(alpha_param, id = 1)
  testthat::expect_true(beautier:::is_init_param(alpha_param))
})
