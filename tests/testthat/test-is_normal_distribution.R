context("is_normal_distribution")

test_that("use", {

  testthat::expect_true(
    beautier::is_normal_distribution(
      beautier::create_normal_distribution()
    )
  )

  testthat::expect_false(beautier::is_normal_distribution("nonsense"))
  testthat::expect_false(beautier::is_normal_distribution(42))
  testthat::expect_false(beautier::is_normal_distribution(NA))
  testthat::expect_false(beautier::is_normal_distribution(NULL))
})
