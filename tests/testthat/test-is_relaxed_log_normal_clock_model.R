context("is_relaxed_log_normal_clock_model")

test_that("use", {

  testthat::expect_true(
    is_relaxed_log_normal_clock_model(
      create_clock_model(name = "relaxed_log_normal")
    )
  )
  testthat::expect_false(
    is_relaxed_log_normal_clock_model(
      create_clock_model(name = "strict")
    )
  )

})
