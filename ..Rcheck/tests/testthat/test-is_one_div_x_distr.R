context("is_one_div_x_distr")

test_that("use", {

  testthat::expect_true(
    is_one_div_x_distr(
      beautier::create_one_div_x_distr()
    )
  )

  testthat::expect_false(is_one_div_x_distr("nonsense"))
  testthat::expect_false(is_one_div_x_distr(42))
  testthat::expect_false(is_one_div_x_distr(NA))
  testthat::expect_false(is_one_div_x_distr(NULL))
})
