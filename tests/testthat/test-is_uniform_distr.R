context("is_uniform_distr")

test_that("use", {

  testthat::expect_true(
    is_uniform_distr(
      beautier::create_uniform_distr()
    )
  )

  testthat::expect_false(is_uniform_distr("nonsense"))
  testthat::expect_false(is_uniform_distr(42))
  testthat::expect_false(is_uniform_distr(NA))
  testthat::expect_false(is_uniform_distr(NULL))
})
