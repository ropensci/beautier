context("test-create_site_models_from_names")

test_that("use", {

  names <- get_site_model_names()
  site_models <- create_site_models_from_names(names)
  # Indexed use
  for (i in seq_along(names)) {
    expect_equal(names[i], site_models[[i]]$name)
  }
})
