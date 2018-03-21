context("create_beast2_input_loggers")

test_that("use", {

  testthat::expect_silent(
    beautier:::create_beast2_input_loggers(
      ids = "test_output_0"
    )
  )

})

test_that("abuse", {

  ids <- c("anthus_nd2", "anthus_aco.fas")

  # Two filenames, one site model
  testthat::expect_error(
    create_beast2_input_loggers(
      ids = ids,
      site_models = list(create_jc69_site_model())
    )
  )

  # Two filenames, one clock model
  testthat::expect_error(
    create_beast2_input_loggers(
      ids = ids,
      clock_models = list(create_strict_clock_model())
    )
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_loggers(
      ids = ids,
      tree_priors = list(create_yule_tree_prior())
    )
  )

})
