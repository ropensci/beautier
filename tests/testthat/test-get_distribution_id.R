context("get_distribution_id")

test_that("use", {

  testthat::expect_silent(
    get_distribution_id(create_uniform_distr())
  )

})

test_that("abuse", {

  testthat::expect_error(get_distribution_id("nonsense"))
  testthat::expect_error(get_distribution_id(NULL))
  testthat::expect_error(get_distribution_id(NA))

})
