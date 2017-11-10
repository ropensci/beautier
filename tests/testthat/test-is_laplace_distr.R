context("is_laplace_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_laplace_distr(
      beautier::create_laplace_distr()
    )
  )

  testthat::expect_false(beautier::is_laplace_distr("nonsense"))
  testthat::expect_false(beautier::is_laplace_distr(42))
  testthat::expect_false(beautier::is_laplace_distr(NA))
  testthat::expect_false(beautier::is_laplace_distr(NULL))
})
