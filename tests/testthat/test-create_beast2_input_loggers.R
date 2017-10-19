context("create_beast2_input_loggers")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_loggers(
      ids = "test_output_0"
    )
  )

})
