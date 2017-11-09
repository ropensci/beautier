context("is_alpha_parameter")

test_that("use", {

  testthat::expect_true(
    is_alpha_parameter(
      create_alpha_parameter()
    )
  )

})
