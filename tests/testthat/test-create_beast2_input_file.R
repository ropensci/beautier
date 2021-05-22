test_that("use", {

  output_filename <- get_beautier_tempfilename()
  expect_false(file.exists(output_filename))
  expect_silent(
    create_beast2_input_file(
      get_fasta_filename(),
      output_filename
    )
  )
  expect_true(file.exists(output_filename))
  file.remove(output_filename)
})

test_that("abuse", {

  # input_filenames
  expect_error(
    create_beast2_input_file(
      input_filenames = "nonexisting", # Error
      output_filename = "irrelevant"
    ),
    "'input_filenames' is deprecated, use 'input_filename' instead"
  )

  # 2 site_model
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      site_models = "nonsense"
    ),
    "'site_models' is deprecated, use 'site_model' instead"
  )

  # 3 clock_models
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      clock_models = "nonsense"
    ),
    "'clock_models' is deprecated, use 'clock_model' instead"
  )

  # 4 tree_prior
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      tree_priors = "nonsense"
    ),
    "'tree_priors' is deprecated, use 'tree_prior' instead"
  )
  # mrca_priors
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      mrca_priors = "nonsense"
    ),
    "'mrca_priors' is deprecated, use 'mrca_prior' instead."
  )
  # 7 posterior_crown_age
  expect_error(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = "irrelevant",
      posterior_crown_age = "nonsense" # Error
    ),
    "'posterior_crown_age' is deprecated"
  )

  # will create folders needed to hold file
  output_filename <- file.path(
    get_beautier_tempfilename(), "1", "2", "beast2.xml"
  )
  expect_silent(
    create_beast2_input_file(
      input_filename = get_fasta_filename(),
      output_filename = output_filename
    )
  )
  file.remove(output_filename)
  unlink(dirname(dirname(dirname(output_filename))), recursive = TRUE)

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
