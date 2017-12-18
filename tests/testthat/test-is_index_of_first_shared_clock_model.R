context("is_index_of_first_shared_clock_model")

test_that("use", {

  a <- create_strict_clock_model(id = "a")
  b <- create_rln_clock_model(id = "b")
  aa <- list(a, a) # shared
  ab <- list(a, b)

  testthat::expect_true(beautier:::is_index_of_first_shared_clock_model(1, aa))
  testthat::expect_false(beautier:::is_index_of_first_shared_clock_model(2, aa))
  testthat::expect_false(beautier:::is_index_of_first_shared_clock_model(1, ab))
})
