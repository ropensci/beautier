context("get_bd_birth_rate_distr")

test_that("use", {

  testthat::expect_silent(
    get_bd_birth_rate_distr(create_bd_tree_prior())
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_bd_birth_rate_distr(create_yule_tree_prior())
  )

})
