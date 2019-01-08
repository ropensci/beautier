context("test-create_clock_model_from_name")

test_that("use", {
  names <- c("strict", "relaxed_log_normal")
  for (name in names) {
    expect_equal(name, create_clock_model_from_name(name)$name)
  }
})
