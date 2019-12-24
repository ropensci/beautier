context("init_distr")

test_that("use", {

  testthat::expect_silent(
    init_distr(create_uniform_distr())
  )

})
