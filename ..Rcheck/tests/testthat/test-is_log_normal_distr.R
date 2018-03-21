context("is_log_normal_distr")

test_that("use", {

  testthat::expect_true(
    is_log_normal_distr(
      beautier::create_log_normal_distr()
    )
  )

  testthat::expect_false(is_log_normal_distr("nonsense"))
  testthat::expect_false(is_log_normal_distr(42))
  testthat::expect_false(is_log_normal_distr(NA))
  testthat::expect_false(is_log_normal_distr(NULL))

})
