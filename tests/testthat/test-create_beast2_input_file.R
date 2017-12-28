context("create_beast2_input_file")

test_that("use", {

  output_xml_filename <- tempfile()
  testit::assert(!file.exists(output_xml_filename))

  testthat::expect_silent(
    create_beast2_input_file(
      get_fasta_filename(),
      output_xml_filename
    )
  )

  testthat::expect_true(file.exists(output_xml_filename))
})
