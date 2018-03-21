context("is_poisson_distr")

test_that("use", {

  testthat::expect_true(
    is_poisson_distr(
      beautier::create_poisson_distr()
    )
  )

  testthat::expect_false(is_poisson_distr("nonsense"))
  testthat::expect_false(is_poisson_distr(42))
  testthat::expect_false(is_poisson_distr(NA))
  testthat::expect_false(is_poisson_distr(NULL))
})
