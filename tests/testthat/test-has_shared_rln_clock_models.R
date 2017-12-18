context("has_shared_rln_clock_models")

test_that("use, two", {

  a <- create_rln_clock_model(id = "a")
  b <- create_rln_clock_model(id = "b")
  aa <- list(a, a) # Shared
  ab <- list(a, b) # Unlinked
  ba <- list(b, a) # Unlinked
  testthat::expect_true(
    beautier:::has_shared_rln_clock_models(aa)
  )
  testthat::expect_false(
    beautier:::has_shared_rln_clock_models(ab)
  )
  testthat::expect_false(
    beautier:::has_shared_rln_clock_models(ba)
  )
})
