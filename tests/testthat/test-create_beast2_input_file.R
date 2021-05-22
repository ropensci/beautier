context("create_beast2_input_file")

test_that("use", {

  output_filename <- get_beautier_tempfilename()
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

  output_filename <- get_beautier_tempfilename()

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

  # will create folders needed to hold file
  expect_silent(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = file.path(
        get_beautier_tempfilename(), "1", "2", "beast2.xml"
      )
    )
  )

  # output filename is invalid
  if (rappdirs::app_dir()$os != "win") {
    expect_error(
      create_beast2_input_file(
        input_filename = get_fasta_filename(),
        output_filename = "/no/way",
      ),
      "Cannot write to file with name '/no/way'"
    )
  }
})

test_that("cannot create CBS with less than 6 taxa", {
  # Tested by 'check_file_and_model_agree'
})
