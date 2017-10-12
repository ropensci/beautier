context("test_create_beast2_input")

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

test_that("Can specify fixed crown age", {
  input_fasta_filename <- beastscriptr::get_input_fasta_filename()
  output_xml_filename_fixed <- tempfile()
  output_xml_filename_nonfixed <- tempfile()
  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  created_lines_fixed <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_age = TRUE,
    initial_phylogeny = beastscriptr::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )

  created_lines_nonfixed <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename_nonfixed
  )

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

test_that("Check that birth_death_2_4.xml is reproduced", {
  # Creates an XML file from a known-to-be-valid input file
  # and tests if this identical to a known-to-be-valid XML output file
  input_fasta_filenames <- get_input_fasta_filename()
  output_xml_filename <-  basename(get_output_xml_filename())
  # Input file must be found
  testthat::expect_true(file.exists(input_fasta_filename))

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = output_xml_filename
  )

  expected_lines <- readLines(get_output_xml_filename())
  testthat::expect_equal(expected_lines[1], created_lines[1])
  testthat::expect_equal(expected_lines[2], created_lines[2])
  skip("Found upstream bug")
  write.csv(created_lines, "~/created.csv")
  write.csv(expected_lines, "~/expected.csv")
  testthat::expect_equal(expected_lines[5], created_lines[5])
  testthat::expect_equal(expected_lines[7], created_lines[7])
  for (i in 1:120) {
    print(i)
    testthat::expect_equal(
      expected_lines[i], created_lines[i]
    )
  }

  testthat::expect_identical(created_lines, expected_lines)
})


test_that("Produce XML for Yule species tree prior", {
  skip("First refactor")
  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000000,
    tree_priors = create_tree_prior(name = "yule"),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(output_xml_filename)
  )
})
