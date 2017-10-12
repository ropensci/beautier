context("create_beast2_input")

test_that("checks input", {

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = "nonexisting", # Error
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml"
    )
  )
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 0, # Error
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "nonsense"),
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml",
      fixed_crown_age = "nonsense" # Error
    )
  )

})

test_that("Check that birth_death_2_4.xml is reproduced", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filenames <- beastscriptr::get_input_fasta_filename()
  output_xml_filename <-  basename(beastscriptr::get_output_xml_filename())
  # Input file must be found
  testthat::expect_true(file.exists(input_fasta_filenames))

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    mcmc_chainlength = 10000000,
    tree_priors = beastscriptr::create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename
  )

  expected_lines <- readLines(beastscriptr::get_output_xml_filename())
  testthat::expect_identical(created_lines, expected_lines)
})




test_that("Check that coalescent_constant_population_2_4.xml is reproduced", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filenames <- beastscriptr::get_input_fasta_filename()

  # Input file must be found
  testthat::expect_true(file.exists(input_fasta_filenames))

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    mcmc_chainlength = 10000000,
    tree_priors = beastscriptr::create_tree_prior(name = "coalescent_constant_population"),
    output_xml_filename = output_xml_filename
  )

  expected_lines <- readLines(system.file("extdata", "coalescent_constant_population_2_4.xml", package = "beastscriptr"))

  testthat::expect_equal(expected_lines[1], created_lines[1])
  testthat::expect_equal(expected_lines[2], created_lines[2])
  skip("WIP")
  write.csv(created_lines, "~/created.csv")
  write.csv(expected_lines, "~/expected.csv")
  testthat::expect_equal(expected_lines[5], created_lines[5])
  testthat::expect_equal(expected_lines[41], created_lines[41])
  for (i in 1:120) {
    print(i)
    testthat::expect_equal(
      expected_lines[i], created_lines[i]
    )
}

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that yule_2_4.xml is reproduced", {
  skip("Do coalescent_constant_population first ")
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filenames <- beastscriptr::get_input_fasta_filename()
  output_xml_filename <- system.file("extdata", "yule_2_4.xml", package = "beastscriptr")

  # Input file must be found
  testthat::expect_true(file.exists(input_fasta_filenames))

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    mcmc_chainlength = 10000000,
    tree_priors = beastscriptr::create_tree_prior(name = "yule"),
    output_xml_filename = output_xml_filename
  )

  expected_lines <- readLines(beastscriptr::get_output_xml_filename())
  testthat::expect_identical(created_lines, expected_lines)
})
