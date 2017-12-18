context("get_clock_models_ids")

test_that("use", {

  a <- create_strict_clock_model(id = "a")
  b <- create_rln_clock_model(id = "b")

  testthat::expect_equal(
    beautier:::get_clock_models_ids(list(a, b)),
    c("a", "b")
  )

})
