context("init_distr")

test_that("use", {

  testthat::expect_silent(
    beautier:::init_distr(create_uniform_distr())
  )

})
