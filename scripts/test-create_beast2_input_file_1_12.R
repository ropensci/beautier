context("create_beast2_input_file_1_12")

test_that("checks input", {

  # Don't: input is checked by 'create_beast2_input_1_12'
  # See 'create_beast2_input_1_12' tests
})

test_that("Can specify fixed crown age", {

  if (!beautier:::is_on_travis()) return()

  input_fasta_filename <- beautier::get_fasta_filename()
  output_xml_filename_fixed <- tempfile()

  beautier::create_beast2_input_file_1_12(
    input_fasta_filenames = input_fasta_filename,
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_ages = TRUE,
    initial_phylogenies = beautier::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )
  testthat::expect_true(
    lumier::is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("Can specify fixed crown ages", {

  if (!beautier:::is_on_travis()) return()

  input_fasta_filenames <- get_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
  output_xml_filename_fixed <- tempfile()

  beautier::create_beast2_input_file_1_12(
    input_fasta_filenames = input_fasta_filenames,
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_ages = c(TRUE, TRUE),
    initial_phylogenies = beautier::fastas_to_phylos(
      input_fasta_filenames, crown_age = 15)
  )
  testthat::expect_true(
    lumier::is_beast2_input_file(output_xml_filename_fixed)
  )
})
