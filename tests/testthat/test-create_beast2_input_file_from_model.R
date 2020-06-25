test_that("use", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  expect_silent(
    create_beast2_input_file_from_model(
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
    create_beast2_input_file_from_model(
      input_filename = "nonexisting", # Error
      output_filename
    ),
    "'input_filename' not found"
  )

  # 2 site_model
  expect_error(
    create_beast2_input_file_from_model(
      input_filename = get_fasta_filename(),
      output_filename,
      inference_model = "nonsense"
    ),
    "'inference_model' must be an inference model"
  )
})

test_that("cannot create CBS with less than 6 taxa", {
  # Tested by 'check_file_and_model_agree'
})
