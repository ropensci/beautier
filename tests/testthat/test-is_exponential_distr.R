context("is_exponential_distr")

test_that("use", {

  testthat::expect_true(
    is_exponential_distr(
      create_exponential_distr()
    )
  )

  testthat::expect_false(is_exponential_distr("nonsense"))
  testthat::expect_false(is_exponential_distr(NA))
  testthat::expect_false(is_exponential_distr(NULL))

})
