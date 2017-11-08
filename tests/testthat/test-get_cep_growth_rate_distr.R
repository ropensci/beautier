context("get_cep_growth_rate_distr")

test_that("use", {

  testthat::expect_silent(
    get_cep_growth_rate_distr(create_cep_tree_prior())
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_cep_growth_rate_distr(create_yule_tree_prior())
  )

})
