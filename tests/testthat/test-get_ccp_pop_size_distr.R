context("get_ccp_pop_size_distr")

test_that("use", {

  testthat::expect_silent(
    get_ccp_pop_size_distr(create_ccp_tree_prior())
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_ccp_pop_size_distr(create_yule_tree_prior())
  )

})
