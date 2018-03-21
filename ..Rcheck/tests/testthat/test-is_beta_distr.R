context("is_beta_distr")

test_that("use", {

  testthat::expect_true(
    is_beta_distr(
      beautier::create_beta_distr()
    )
  )

  testthat::expect_false(is_beta_distr("nonsense"))
  testthat::expect_false(is_beta_distr(42))
  testthat::expect_false(is_beta_distr(NA))
  testthat::expect_false(is_beta_distr(NULL))

})
