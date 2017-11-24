context("create_param")

test_that("abuse", {

  testthat::expect_error(
    create_param(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})
