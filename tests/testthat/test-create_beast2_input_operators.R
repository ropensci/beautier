test_that("abuse", {

  id <- "test_output_0"
  expect_silent(
    create_beast2_input_operators(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model(id = id)),
      tree_priors = list(create_yule_tree_prior(id = id)),
      fixed_crown_ages = FALSE
    )
  )
})
