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
