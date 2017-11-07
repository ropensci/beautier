context("get_distribution_id")

test_that("use", {

  testthat::expect_silent(
    get_distribution_id(create_uniform_distr())
  )
})
