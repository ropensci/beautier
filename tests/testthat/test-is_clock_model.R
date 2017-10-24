context("is_clock_model")

test_that("use", {

  testthat::expect_true(is_clock_model(list(name = "strict")))
  testthat::expect_false(is_clock_model("nonsense"))
  testthat::expect_false(is_clock_model(list(name = "nonsense")))
})
