context("create_beast2_input_loggers")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_loggers(
      ids = "test_output_0"
    )
  )

})

test_that("abuse", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  ids <- get_ids(c(fasta_filename_1, fasta_filename_2))

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
