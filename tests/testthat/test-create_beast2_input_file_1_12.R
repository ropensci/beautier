context("create_beast2_input_file_1_12")

test_that("use", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  expect_silent(
    create_beast2_input_file_1_12(
      get_fasta_filename(),
      output_filename
    )
  )
  expect_true(file.exists(output_filename))
})
