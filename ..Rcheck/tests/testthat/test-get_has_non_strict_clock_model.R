context("get_has_non_strict_clock_model")

test_that("use", {

  n <- list(create_rln_clock_model())
  s <- list(create_strict_clock_model())
  nn <- list(create_rln_clock_model(), create_rln_clock_model())
  ns <- list(create_rln_clock_model(), create_strict_clock_model())
  sn <- list(create_strict_clock_model(), create_rln_clock_model())
  ss <- list(create_strict_clock_model(), create_strict_clock_model())

  # true if there is an n in the name
  testthat::expect_true(beautier:::get_has_non_strict_clock_model(n))
  testthat::expect_false(beautier:::get_has_non_strict_clock_model(s))
  testthat::expect_true(beautier:::get_has_non_strict_clock_model(nn))
  testthat::expect_true(beautier:::get_has_non_strict_clock_model(ns))
  testthat::expect_true(beautier:::get_has_non_strict_clock_model(sn))
  testthat::expect_false(beautier:::get_has_non_strict_clock_model(ss))

})
