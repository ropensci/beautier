context("is_alphaparam")

test_that("use", {

  testthat::expect_true(
    is_alphaparam(
      create_alphaparam()
    )
  )

})
