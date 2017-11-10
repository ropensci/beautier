context("is_inv_gamma_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_inv_gamma_distr(
      beautier::create_inv_gamma_distr()
    )
  )

  testthat::expect_false(beautier::is_inv_gamma_distr("nonsense"))
  testthat::expect_false(beautier::is_inv_gamma_distr(42))
  testthat::expect_false(beautier::is_inv_gamma_distr(NA))
  testthat::expect_false(beautier::is_inv_gamma_distr(NULL))
})
