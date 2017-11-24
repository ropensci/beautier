context("is_exp_distr")

test_that("use", {

  testthat::expect_true(
    is_exp_distr(
      create_exp_distr()
    )
  )

  testthat::expect_false(is_exp_distr("nonsense"))
  testthat::expect_false(is_exp_distr(NA))
  testthat::expect_false(is_exp_distr(NULL))

})
