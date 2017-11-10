context("get_distr_id")

test_that("use", {

  testthat::expect_silent(
    get_distr_id(create_uniform_distr())
  )

})

test_that("abuse", {

  testthat::expect_error(get_distr_id("nonsense"))
  testthat::expect_error(get_distr_id(NULL))
  testthat::expect_error(get_distr_id(NA))

})
