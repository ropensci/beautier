context("is_id")

test_that("use", {

  testthat::expect_true(is_id("anthus_aco"))
  testthat::expect_true(is_id(3))
  testthat::expect_false(is_id(ape::rcoal(3)))
  testthat::expect_false(is_id(NULL))
  testthat::expect_false(is_id(NA))

})
