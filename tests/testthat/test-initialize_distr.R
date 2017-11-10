context("initialize_distr")

test_that("use", {

  testthat::expect_silent(
    beautier:::initialize_distr(create_uniform_distr())
  )

})
