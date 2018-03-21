context("is_alpha_param")

test_that("use", {

  testthat::expect_true(
    is_alpha_param(
      create_alpha_param()
    )
  )

})
