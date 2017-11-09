context("initialize_distr")

test_that("use", {

  testthat::expect_silent(
    initialize_distr(create_uniform_distr())
  )

})
