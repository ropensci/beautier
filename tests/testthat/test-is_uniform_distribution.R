context("is_uniform_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_uniform_distr(
      beautier::create_uniform_distr()
    )
  )

  testthat::expect_false(beautier::is_uniform_distr("nonsense"))
  testthat::expect_false(beautier::is_uniform_distr(42))
  testthat::expect_false(beautier::is_uniform_distr(NA))
  testthat::expect_false(beautier::is_uniform_distr(NULL))
})
