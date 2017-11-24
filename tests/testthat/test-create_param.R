context("createparam")

test_that("abuse", {

  testthat::expect_error(
    createparam(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})
