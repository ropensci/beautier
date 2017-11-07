context("is_log_normal_distribution")

test_that("use", {

  testthat::expect_true(
    beautier::is_log_normal_distribution(
      beautier::create_log_normal_distribution()
    )
  )
})
