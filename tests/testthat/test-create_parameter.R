context("create_parameter")

test_that("abuse", {

  testthat::expect_error(
    create_parameter(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})
