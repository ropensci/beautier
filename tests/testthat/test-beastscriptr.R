context("test_beastscriptr")

test_that("checks input", {

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



test_that("Check that test_output_0.xml is reproduced by beastscriptr", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <-  tempfile()
  expected_output_xml_filename <- get_output_xml_filename()
  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)
  # To-be-created output file must be absent
  testthat::expect_equal(file.exists(output_xml_filename), FALSE)
  # Expected file must be present
  testthat::expect_equal(file.exists(expected_output_xml_filename), TRUE)

  beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  testthat::expect_equal(file.exists(output_xml_filename), TRUE)
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

# Runs BEAST2
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

  # The output file created when it BEAST2 can run
  # (which only happens if the input is valid)
  output_xml_state_filename <- basename(paste0(output_xml_filename, ".state"))

  testthat::expect_false(file.exists(output_xml_filename))
  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(output_xml_filename)
  )

})

test_that("Runs BEAST2, BD species tree prior, fixed crown age, random tree", {

  setwd(path.expand("~"))
  set.seed(42)

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

  # Run BEAST2 to measure posterior
  testthat::expect_false(file.exists(beast_state_filename))
  testthat::expect_false(file.exists(beast_log_filename))
  testthat::expect_false(file.exists(beast_trees_filename))
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar",
    " -statefile ", beast_state_filename,
    " -overwrite", beast_filename
  )
  verbose <- FALSE
  if (!verbose) {
    cmd <- paste(cmd, "1>/dev/null 2>/dev/null")
  }
  system(cmd)
  # If these are absent, BEAST2 could not parse the input file
  testthat::expect_true(file.exists(beast_state_filename))
  testthat::expect_true(file.exists(beast_log_filename))
  testthat::expect_true(file.exists(beast_trees_filename))

  # All TreeHeights (crown ages) should be the same
  posterior <- RBeast::parse_beast_posterior(
    trees_filename = beast_trees_filename,
    log_filename = beast_log_filename)
  testthat::expect_true(all(posterior$estimates$TreeHeight
    == posterior$estimates$TreeHeight[1]))

  file.remove(beast_filename)
  file.remove(beast_state_filename)
})


test_that(paste0("Runs BEAST2, BD species tree prior, fixed crown age, ",
  "specified tree"), {

  setwd(path.expand("~"))
  set.seed(43)

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


  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_prior = "birth_death",
    output_xml_filename = beast_filename,
    fixed_crown_age = TRUE,
    initial_phylogeny = beastscriptr::fasta_to_phylo(
      input_fasta_filename,
      crown_age = crown_age)

  )
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(beast_filename)
  )

  # Run BEAST2 to measure posterior
  testthat::expect_false(file.exists(beast_state_filename))
  testthat::expect_false(file.exists(beast_log_filename))
  testthat::expect_false(file.exists(beast_trees_filename))
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar",
    " -statefile ", beast_state_filename,
    " -overwrite", beast_filename
  )
  verbose <- FALSE
  if (!verbose) {
    cmd <- paste(cmd, "1>/dev/null 2>/dev/null")
  }
  system(cmd)
  # If these are absent, BEAST2 could not parse the input file
  testthat::expect_true(file.exists(beast_state_filename))
  testthat::expect_true(file.exists(beast_log_filename))
  testthat::expect_true(file.exists(beast_trees_filename))

  # All TreeHeights (crown ages) should be the same as specified
  posterior <- RBeast::parse_beast_posterior(
    trees_filename = beast_trees_filename,
    log_filename = beast_log_filename)
  testthat::expect_equal(posterior$estimates$TreeHeight[1], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(posterior$estimates$TreeHeight[10], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(crown_age,
    beastscriptr::get_phylogeny_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
  file.remove(beast_filename)
  file.remove(beast_state_filename)
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
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_age = TRUE,
    initial_phylogeny = beastscriptr::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )
  testthat::expect_equal(file.exists(output_xml_filename_fixed), TRUE)
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(output_xml_filename_fixed)
  )

  beastscriptr::beast_scriptr(
    input_fasta_filename = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_prior = "birth_death",
    output_xml_filename = output_xml_filename_nonfixed
  )
  testthat::expect_equal(file.exists(output_xml_filename_nonfixed), TRUE)
  testthat::expect_true(
    beastscriptr::is_valid_beast2_input_file(output_xml_filename_nonfixed)
  )

  created_lines_fixed <- readLines(output_xml_filename_fixed)
  created_lines_nonfixed <- readLines(output_xml_filename_nonfixed)

  # treeScaler operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "<operator id=\"treeScaler",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "<operator id=\"treeScaler",
      x = created_lines_fixed))
  )

  # wide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "<operator id=\"treeRootScaler",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "<operator id=\"treeRootScaler",
      x = created_lines_fixed))
  )

  # subtree slide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "<operator id=\"SubtreeSlide",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "<operator id=\"SubtreeSlide",
      x = created_lines_fixed))
  )

  # Lines below must be absent when a starting tree is given
  # <init estimate="false" id="RandomTree.t:xxx" initial="@Tree.t:xxx" spec="beast.evolution.tree.RandomTree" taxa="@xxx"> # nolint
  # </init>
  testthat::expect_equal(1,
    length(grep(pattern = "<init id=\"RandomTree.t:.*estimate=\"false\"",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "<init id=\"RandomTree.t:.*estimate=\"false\"",
      x = created_lines_fixed))
  )
})
