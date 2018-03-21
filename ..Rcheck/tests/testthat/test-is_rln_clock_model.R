context("is_rln_clock_model")

test_that("use", {

  testthat::expect_true(
    is_rln_clock_model(
      create_rln_clock_model()
    )
  )
  testthat::expect_false(
    is_rln_clock_model(
      create_strict_clock_model()
    )
  )

})
