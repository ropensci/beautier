context("is_valid_beast2_input_file")

test_that("can detect valid file", {

  # Simulate a random alignment and save it to a FASTA file
  n_taxa <- 5
  sequence_length <- 10
  input_fasta_filename <- tempfile(
    pattern = "beast_scriptr_test_",
    fileext = ".fas"
  )
  testthat::expect_false(file.exists(input_fasta_filename))
  beastscriptr::create_random_fasta(
    n_taxa = n_taxa,
    sequence_length = sequence_length,
    filename = input_fasta_filename
  )
  testthat::expect_true(file.exists(input_fasta_filename))

  # Create XML file from that
  output_xml_filename <- tempfile(
    pattern = "beast_scriptr_test_3_",
    fileext = ".xml"
  )


  testthat::expect_false(file.exists(output_xml_filename))
  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(file.exists(output_xml_filename))
  testthat::expect_true(is_valid_beast2_input_file(output_xml_filename))

  # If these are absent, BEAST2 could not read the input file
  testthat::expect_true(file.exists(output_xml_filename))

})

test_that("testing FASTA file is not a valid BEAST2 input file", {
  testthat::expect_false(
    is_valid_beast2_input_file(
      beastscriptr::get_input_fasta_filename()
    )
  )
})

test_that("testing BEAST2 input file is a valid BEAST2 input file", {

  testthat::expect_true(
    is_valid_beast2_input_file(
      beastscriptr::get_output_xml_filename()
    )
  )

})

