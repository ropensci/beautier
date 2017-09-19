context("test_beastscriptr")

test_that("checks input", {

  expect_error(
    beast_scriptr(
      input_fasta_filename = "nonexisting", # Error
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      output_xml_filename = "output.xml"
    )
  )
  expect_error(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 0, # Error
      tree_prior = "birth_death",
      output_xml_filename = "output.xml"
    )
  )
  expect_error(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_prior = "nonsense", # Error
      output_xml_filename = "output.xml"
    )
  )
})

test_that("beastscriptr: produce a file for birth-death?", {

  # Creates an XML file from a known-to-be-valid input file
  input_fasta_filename <- get_input_fasta_filename()
  expect_equal(file.exists(input_fasta_filename), TRUE)
  output_xml_filename <- tempfile()
  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)
  file.remove(output_xml_filename)
  expect_equal(file.exists(output_xml_filename), FALSE)
})

test_that("beastscriptr produces a file for coalescent_constant_population?", {

  # Creates an XML file from a known-to-be-valid input file
  input_fasta_filename <- get_input_fasta_filename()
  expect_equal(file.exists(input_fasta_filename), TRUE)
  output_xml_filename <- tempfile()
  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "coalescent_constant_population",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)
  file.remove(output_xml_filename)
  expect_equal(file.exists(output_xml_filename), FALSE)
})



test_that("Check that test_output_0.xml is reproduced by beastscriptr", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <-  tempfile()
  expected_output_xml_filename <- get_output_xml_filename()
  # Input file must be found
  expect_equal(file.exists(input_fasta_filename), TRUE)
  # To-be-created output file must be absent
  expect_equal(file.exists(output_xml_filename), FALSE)
  # Expected file must be present
  expect_equal(file.exists(expected_output_xml_filename), TRUE)

  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)
  created_lines <- readLines(output_xml_filename)
  expected_lines <- readLines(expected_output_xml_filename)
  if (!identical(created_lines, expected_lines)) {
    save_text(filename = "created.txt", text = created_lines)
    save_text(filename = "expected.txt", text = expected_lines)
  } else {
    expect_identical(created_lines, expected_lines)
    file.remove(filename = "created.txt")
    file.remove(filename = "expected.txt")
    file.remove(filename = output_xml_filename)
  }
  file.remove(filename = "created.txt")
  file.remove(filename = "expected.txt")
})

test_that("Test if input file can be read by BEAST2", {
  # Creates an XML file from a generated FASTA file

  # Create FASTA file
  input_fasta_filename <- tempfile(
    pattern = "beast_scriptr_test_",
    fileext = ".fas"
  )
  # Input file must not be present,
  # otherwise BEAST2 will prompt the user when creating .trees files
  expect_equal(file.exists(input_fasta_filename), FALSE)

  n_taxa <- 5
  sequence_length <- 10
  create_random_fasta(
    n_taxa = n_taxa,
    sequence_length = sequence_length,
    filename = input_fasta_filename
  )

  # Create XML file from that
  output_xml_filename <- tempfile(
    pattern = "beast_scriptr_test_3_",
    fileext = ".xml"
  )

  # The output file created when it BEAST2 can run
  # (which only happens if the input is valid)
  output_xml_state_filename <- basename(paste0(output_xml_filename, ".state"))

  # Input file must be found now
  expect_equal(file.exists(input_fasta_filename), TRUE)
  # Output file must not be present, otherwise BEAST2 will prompt the user
  expect_equal(file.exists(output_xml_filename), FALSE)

  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  expect_equal(file.exists(output_xml_filename), TRUE)

  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar",
    output_xml_filename, "1>/dev/null 2>/dev/null"
  )
  system(cmd)

  expect_true(file.exists(output_xml_filename))
  expect_true(file.exists(output_xml_state_filename))

  file.remove(output_xml_filename)
  file.remove(output_xml_state_filename)
})




test_that("Can specify fixed crown age", {
  input_fasta_filename <- beastscriptr::get_input_fasta_filename()
  output_xml_filename_fixed <- tempfile()
  output_xml_filename_nonfixed <- tempfile()
  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)
  # To-be-created output file must be absent
  testthat::expect_equal(file.exists(output_xml_filename_fixed), FALSE)
  testthat::expect_equal(file.exists(output_xml_filename_nonfixed), FALSE)


  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename_fixed
  )
  testthat::expect_equal(file.exists(output_xml_filename_fixed), TRUE)

  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename_nonfixed
  )
  testthat::expect_equal(file.exists(output_xml_filename_nonfixed), TRUE)

  created_lines_fixed <- readLines(output_xml_filename_fixed)
  created_lines_nonfixed <- readLines(output_xml_filename_nonfixed)

  beastscriptr::save_text(filename = "~/created.txt", text = created_lines_fixed)
  print(created_lines_fixed)
})
