context("test_beastscriptr")

## TODO: Rename context
## TODO: Add more tests

test_that("test_output_0.fas can be found", {
  filename <- "beastscriptr/data/test_output_0.fas"
  expect_equal(file.exists(filename), TRUE)
})

test_that("birth_death_0_20151005.xml can be found", {
  filename <- "birth_death_0_20151005.xml"
  expect_equal(file.exists(filename), TRUE)
})

test_that("beastscriptr checks for valid inputs", {
  expect_error(
    beast_scriptr(
      input_fasta_filename = "nonexisting", # Error
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      date_str = "20160314",
      output_xml_filename = "output.xml"
    )
  )
  expect_error(
    beast_scriptr(
      input_fasta_filename = "existing",
      mcmc_chainlength = 0, # Error
      tree_prior = "birth_death",
      date_str = "20160314",
      output_xml_filename = "output.xml"
    )
  )
  expect_error(
    beast_scriptr(
      input_fasta_filename = "existing",
      mcmc_chainlength = 1000,
      tree_prior = "nonsense", # Error
      date_str = "20160314",
      output_xml_filename = "output.xml"
    )
  )
  expect_error(
    beast_scriptr(
      input_fasta_filename = "existing",
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      date_str = "20160314", # Error
      output_xml_filename = "output.xml"
    )
  )

  expect_error(
    beast_scriptr(
      input_fasta_filename = "existing",
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      date_str = "20160314",
      output_xml_filename = "output.bat" # Error
    )
  )
})

test_that("beastscriptr test #1", {

  # Creates an XML file from a known-to-be-valid input file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- "test_output_0.xml"
  expect_equal(assert(file.exists(input_fasta_filename), TRUE))
  beast_scripter(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    date_str = "20151022",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)
  print("Tested to create an XML file successfully")
})



test_that("beastscriptr test #2", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- "test_output_0.xml"
  expected_output_xml_filename <- get_output_xml_filename()
  expect_equal(file.exists(input_fasta_filename), TRUE) # Input file must be found
  expect_equal(file.exists(expected_output_xml_filename), TRUE) # Expected file must be present

  beast_scripter(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    date_str = "20151022",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)
  created_lines <- readLines(output_xml_filename)
  expected_lines <- readLines(expected_output_xml_filename)
  if (!identical(created_lines,expected_lines)) {
    save_text(filename = "created.txt", text = created_lines)
    save_text(filename = "expected.txt", text = expected_lines)
  }
  expect_equal(identical(created_lines,expected_lines), TRUE)
})

test_that("beastscriptr test #3", {
  # Creates an XML file from a generated FASTA file

  # Create FASTA file
  input_fasta_filename <- tempfile(
    pattern = "beast_scripter_test_",
    fileext = ".fas"
  )
  expect_equal(file.exists(input_fasta_filename), FALSE) # Input file must not be present, otherwise BEAST2 will prompt the user when creating .trees files
  n_taxa <- 5
  sequence_length <- 10
  create_random_fasta(
    n_taxa = n_taxa,
    sequence_length = sequence_length,
    filename = input_fasta_filename
  )

  # Create XML file from that
  output_xml_filename <- tempfile(
    pattern = "beast_scripter_test_3_",
    fileext = ".xml"
  )

  expect_equal(file.exists(input_fasta_filename), TRUE) # Input file must be found now
  expect_equal(file.exists(output_xml_filename), FALSE) # Output file must not be present, otherwise BEAST2 will prompt the user

  beast_scripter(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    date_str = "20151022",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)

  cmd <- paste("java -jar ~/Programs/beast/lib/beast.jar",output_xml_filename)
  system(cmd)

  print("Tested to create an XML file successfully. If you saw BEAST2 output, it worked!")
})
