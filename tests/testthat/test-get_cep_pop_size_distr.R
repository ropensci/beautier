context("get_cep_pop_size_distr")

test_that("use", {

  testthat::expect_silent(
    get_cep_pop_size_distr(create_cep_tree_prior())
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_cep_pop_size_distr(create_yule_tree_prior())
  )

})
