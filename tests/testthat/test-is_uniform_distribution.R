context("is_uniform_distribution")

test_that("use", {

  testthat::expect_true(
    beautier::is_uniform_distribution(
      beautier::create_uniform_distribution()
    )
  )
})
