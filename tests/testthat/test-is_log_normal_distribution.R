context("is_log_normal_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_log_normal_distr(
      beautier::create_log_normal_distr()
    )
  )
})
