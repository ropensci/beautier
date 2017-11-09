context("get_distr_n_params")

test_that("use", {

  testthat::expect_equal(get_distr_n_params(create_gamma_distr()), 2)

})
