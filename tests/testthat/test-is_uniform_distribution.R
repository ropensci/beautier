context("is_uniform_distribution")

test_that("use", {

  testthat::expect_true(
    beautier::is_uniform_distribution(
      beautier::create_uniform_distr()
    )
  )

  testthat::expect_false(beautier::is_uniform_distribution("nonsense"))
  testthat::expect_false(beautier::is_uniform_distribution(42))
  testthat::expect_false(beautier::is_uniform_distribution(NA))
  testthat::expect_false(beautier::is_uniform_distribution(NULL))
})
