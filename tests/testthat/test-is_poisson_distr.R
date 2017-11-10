context("is_poisson_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_poisson_distr(
      beautier::create_poisson_distr()
    )
  )

  testthat::expect_false(beautier::is_poisson_distr("nonsense"))
  testthat::expect_false(beautier::is_poisson_distr(42))
  testthat::expect_false(beautier::is_poisson_distr(NA))
  testthat::expect_false(beautier::is_poisson_distr(NULL))
})
