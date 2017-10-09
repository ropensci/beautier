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

  # Let BEAST2 run the created XML file
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar -validate",
    output_xml_filename
  )
  output <- system(cmd, intern = TRUE)

  # stub: will always be true
  testthat::expect_true(tail(output, n = 1) == "Done!")

  # If these are absent, BEAST2 could not read the input file
  testthat::expect_true(file.exists(output_xml_filename))

})
