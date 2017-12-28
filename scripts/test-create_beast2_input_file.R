context("create_beast2_input_file")

test_that("checks input", {

  # Don't: input is checked by 'create_beast2_input'
  # See 'create_beast2_input' tests
})

test_that("Can specify fixed crown age", {

  input_fasta_filename <- beautier::get_fasta_filename()
  output_xml_filename_fixed <- tempfile()

  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    output_xml_filename = output_xml_filename_fixed,
    posterior_crown_age = 15
  )
  testthat::expect_true(
    lumier::is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("Can specify fixed crown ages", {

  input_fasta_filenames <- get_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
  output_xml_filename_fixed <- tempfile()

  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filenames,
    output_xml_filename = output_xml_filename_fixed,
    posterior_crown_age = 15
  )
  testthat::expect_true(
    lumier::is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("cbs_2_4.xml is invalid", {

  # cbs_2_4.xml is invalid,
  # because the groupSize's dimension is 5 by default,
  # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
  # groupSize cannot be more than 4
  filename <- beautier:::get_path("cbs_2_4.xml")
  testthat::expect_false(lumier::is_beast2_input_file(filename))
})
