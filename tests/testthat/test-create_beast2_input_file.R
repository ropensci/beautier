context("create_beast2_input_file")

test_that("checks input", {

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = "nonexisting", # Error
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml"
    )
  )
  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 0, # Error
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "nonsense"),
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_priors = create_tree_prior(name = "birth_death"),
      output_xml_filename = "output.xml",
      fixed_crown_age = "nonsense" # Error
    )
  )

})

test_that("Produce an XML file for a birth-death species tree prior", {

  # Creates an XML file from a known-to-be-valid input file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )
})

test_that("Produce XML for coalescent constant-population species tree prior", {

  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "coalescent_constant_population"),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )
})

test_that("Runs BEAST2, BD species tree prior, random initial tree", {

  # Simulate a random alignment and save it to a FASTA file
  n_taxa <- 5
  sequence_length <- 10
  input_fasta_filename <- tempfile(
    pattern = "create_beast2_input_file_test_",
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
    pattern = "create_beast2_input_file_test_3_",
    fileext = ".xml"
  )

  # The output file created when it BEAST2 can run
  # (which only happens if the input is valid)
  output_xml_state_filename <- basename(paste0(output_xml_filename, ".state"))

  testthat::expect_false(file.exists(output_xml_filename))
  beastscriptr::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )

})

test_that("A fixed crown age must have equal TreeHeights", {

  posterior <- create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc_chainlength = 10000,
    tree_priors = create_tree_prior(name = "birth_death"),
    fixed_crown_age = TRUE
  )
  testthat::expect_true(all(posterior$estimates$TreeHeight
    == posterior$estimates$TreeHeight[1]))
})


test_that(paste0("Fixed and specified crown age must results in a posterior",
  "with that TreeHeight"), {

  posterior <- create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc_chainlength = 10000,
    fixed_crown_age = TRUE,
    crown_age = 15
  )
  testthat::expect_equal(posterior$estimates$TreeHeight[1], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(posterior$estimates$TreeHeight[10], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(crown_age,
    beastscriptr::get_phylogeny_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
})

test_that("Can specify fixed crown age", {
  input_fasta_filename <- beastscriptr::get_input_fasta_filename()
  output_xml_filename_fixed <- tempfile()

  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  beastscriptr::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_age = TRUE,
    initial_phylogeny = beastscriptr::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("Produce XML for Yule species tree prior", {

  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_tree_prior(name = "yule"),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )
})
