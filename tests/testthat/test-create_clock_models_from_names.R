context("test-create_clock_models_from_names")

test_that("use", {

  names <- get_clock_model_names()
  clock_models <- create_clock_models_from_names(names)
  # Indexed use
  for (i in seq_along(names)) {
    expect_equal(names[i], clock_models[[i]]$name)
  }
})
