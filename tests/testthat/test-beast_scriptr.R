context("beastscriptr")

test_that("checks input", {

  testthat::expect_silent(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    beast_scriptr(
      input_fasta_filename = "nonexisting", # Error
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      output_xml_filename = "output.xml"
    )
  )
  testthat::expect_error(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 0, # Error
      tree_prior = "birth_death",
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_prior = "nonsense", # Error
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    beast_scriptr(
      input_fasta_filename = get_input_fasta_filename(),
      mcmc_chainlength = 1000,
      tree_prior = "birth_death",
      output_xml_filename = "output.xml",
      fixed_crown_age = "nonsense" # Error
    )
  )

})

test_that("Produce an XML file for a birth-death species tree prior", {

  # Creates an XML file from a known-to-be-valid input file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(output_xml_filename)
  )
})

test_that("Produce XML for coalescent constant-population species tree prior", {

  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "coalescent_constant_population",
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(output_xml_filename)
  )
})

test_that("Runs BEAST2, BD species tree prior, random initial tree", {

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
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )

})

test_that("Runs BEAST2, BD species tree prior, fixed crown age, random tree", {

  base_filename <- tempfile(pattern = "beast_scriptr_test_bd_fix_rand_")
  # BEAST2 input XML file, created by beastscriptr::beast_scriptr
  beast_filename <- paste0(base_filename, ".xml")
  # BEAST2 output file, containing the posterior parameter estimates
  beast_log_filename <- paste0(base_filename, ".log")
  # BEAST2 output file, containing the posterior phylogenies
  beast_trees_filename <- paste0(base_filename, ".trees")
  # BEAST2 output file, containing the final MCMC state
  beast_state_filename <- paste0(base_filename, ".xml.state")
  # FASTA file needed only temporarily to store simulated DNA alignments
  input_fasta_filename <- paste0(base_filename, ".fasta")

  # Create FASTA file
  testthat::expect_false(file.exists(input_fasta_filename))
  beastscriptr::create_random_fasta(
    n_taxa = 5,
    sequence_length = 10,
    filename = input_fasta_filename
  )
  testthat::expect_true(file.exists(input_fasta_filename))

  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = beast_filename,
    fixed_crown_age = TRUE
  )
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(beast_filename)
  )
})


test_that(paste0("Runs BEAST2, BD species tree prior, fixed crown age, ",
  "specified tree"), {

  base_filename <- tempfile(pattern = "beast_scriptr_test_bd_fix_spec_")
  # BEAST2 input XML file, created by beastscriptr::beast_scriptr
  beast_filename <- paste0(base_filename, ".xml")
  # BEAST2 output file, containing the posterior parameter estimates
  beast_log_filename <- paste0(base_filename, ".log")
  # BEAST2 output file, containing the posterior phylogenies
  beast_trees_filename <- paste0(base_filename, ".trees")
  # BEAST2 output file, containing the final MCMC state
  beast_state_filename <- paste0(base_filename, ".xml.state")
  # FASTA file needed only temporarily to store simulated DNA alignments
  input_fasta_filename <- paste0(base_filename, ".fasta")

  # Initial phylogeny its crown age.
  # Must be the treeHeights of all posterior's trees
  crown_age <- 15

  # Create FASTA file
  testthat::expect_false(file.exists(input_fasta_filename))
  beastscriptr::create_random_fasta(
    n_taxa = 5,
    sequence_length = 10,
    filename = input_fasta_filename
  )
  testthat::expect_true(file.exists(input_fasta_filename))

  initial_phylogenies <- beastscriptr::fasta_to_phylo(
    fasta_filename = input_fasta_filename,
    crown_age = crown_age)

  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = beast_filename,
    fixed_crown_age = TRUE,
    initial_phylogenies = initial_phylogenies
  )
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(beast_filename)
  )
})
