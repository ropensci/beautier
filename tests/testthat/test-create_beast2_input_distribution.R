context("create_beast2_input_distribution")

test_that("usage", {
  testthat::expect_silent(
    create_beast2_input_distribution(
      ids = "test_output_0"
    )
  )
})
