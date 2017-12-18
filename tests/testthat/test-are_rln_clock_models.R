context("are_rln_clock_models")

test_that("use", {

  rln <- create_rln_clock_model()
  strict <- create_strict_clock_model()

  testthat::expect_equal(
    are_rln_clock_models(
      list(rln, rln)
    ),
    c(TRUE, TRUE)
  )

  testthat::expect_equal(
    are_rln_clock_models(
      list(strict, rln, strict)
    ),
    c(FALSE, TRUE, FALSE)
  )

})
