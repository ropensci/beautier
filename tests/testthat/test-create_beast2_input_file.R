context("create_beast2_input_file")

test_that("use", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  expect_silent(
    create_beast2_input_file(
      get_fasta_filename(),
      output_filename
    )
  )

  expect_true(file.exists(output_filename))
})

test_that("abuse", {

  output_filename <- tempfile()

  # input_filenames
  expect_error(
    create_beast2_input_file(
      input_filenames = "nonexisting", # Error
      output_filename
    ),
    "'input_filenames' is deprecated, use 'input_filename' instead"
  )

  # 2 site_model
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename,
      site_models = "nonsense"
    ),
    "'site_models' is deprecated, use 'site_model' instead"
  )

  # 3 clock_models
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename,
      clock_models = "nonsense"
    ),
    "'clock_models' is deprecated, use 'clock_model' instead"
  )

  # 4 tree_prior
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename,
      tree_priors = "nonsense"
    ),
    "'tree_priors' is deprecated, use 'tree_prior' instead"
  )
  # mrca_priors
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename,
      mrca_priors = "nonsense"
    ),
    "'mrca_priors' is deprecated, use 'mrca_prior' instead."
  )
  # 7 posterior_crown_age
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename,
      posterior_crown_age = "nonsense" # Error
    ),
    "'posterior_crown_age' is deprecated"
  )
})
