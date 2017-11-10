context("is_normal_distr")

test_that("use", {

  testthat::expect_true(
    beautier::is_normal_distr(
      beautier::create_normal_distr()
    )
  )

  testthat::expect_false(beautier::is_normal_distr("nonsense"))
  testthat::expect_false(beautier::is_normal_distr(42))
  testthat::expect_false(beautier::is_normal_distr(NA))
  testthat::expect_false(beautier::is_normal_distr(NULL))
})
