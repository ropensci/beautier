context("brute_force_1")

################################################################################
# Site models
################################################################################

test_that("All site models, clock models and tree priors, crown age est", {

  if (!beautier::is_on_travis()) return()

  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (site_model in beautier::create_site_models()) {
    for (clock_model in beautier::create_clock_models()) {
      for (tree_prior in beautier::create_tree_priors()) {

        output_xml_filename <- tempfile()
        create_beast2_input_file(
          input_fasta_filenames = input_fasta_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          output_xml_filename = output_xml_filename
        )
        is_ok <- beautier::is_beast2_input_file(output_xml_filename)
        testthat::expect_true(is_ok)
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beautier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
        }
      }
    }
  }
})

################################################################################
# Clock models
################################################################################

test_that(paste0("All clock models produce a valid BEAST2 input file, ",
  "fixed crown age"), {

  if (!beautier::is_on_travis()) return()

  clock_models <- beautier::create_clock_models()
  testthat::expect_true(length(clock_models) > 1)

  for (clock_model in clock_models) {
    input_fasta_filename <- get_fasta_filename()
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      clock_models = clock_model,
      output_xml_filename = output_xml_filename,
      fixed_crown_age = TRUE,
      initial_phylogenies = beautier::fasta_to_phylo(
        input_fasta_filename, crown_age = 15)
    )
    testthat::expect_true(
      beautier::is_beast2_input_file(output_xml_filename)
    )
  }
})

################################################################################
# Tree priors
################################################################################

test_that("All tree priors produce a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  tree_priors <- beautier::create_tree_priors()
  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (tree_prior in tree_priors) {

    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      tree_priors = tree_prior,
      output_xml_filename = output_xml_filename
    )
    is_ok <- beautier::is_beast2_input_file(output_xml_filename)
    if (!is_ok) {
      print(tree_prior)
      beautier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    }
    testthat::expect_true(is_ok)
  }
})

test_that(paste0("All tree priors produce a valid BEAST2 input file, ",
  "fixed crown age"), {

  if (!beautier::is_on_travis()) return()

  tree_priors <- beautier::create_tree_priors()
  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (tree_prior in tree_priors) {
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      tree_priors = tree_prior,
      output_xml_filename = output_xml_filename,
      fixed_crown_age = TRUE,
      initial_phylogenies = beautier::fasta_to_phylo(
        input_fasta_filename, crown_age = 15)
    )
    ok <- beautier::is_beast2_input_file(output_xml_filename)
    if (!ok) {
      print(tree_prior$name)
      beautier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    }
    testthat::expect_true(ok)
  }
})

################################################################################
# Combinations
################################################################################

test_that("All site models, clock models and tree priors, fixed crown age", {

  if (!beautier::is_on_travis()) return()

  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (site_model in beautier::create_site_models()) {
    for (clock_model in beautier::create_clock_models()) {
      for (tree_prior in beautier::create_tree_priors()) {

        output_xml_filename <- tempfile()
        create_beast2_input_file(
          input_fasta_filenames = input_fasta_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          output_xml_filename = output_xml_filename,
          fixed_crown_age = TRUE,
          initial_phylogenies = beautier::fasta_to_phylo(
            input_fasta_filename, crown_age = 15)
        )
        is_ok <- beautier::is_beast2_input_file(output_xml_filename)
        testthat::expect_true(is_ok)
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beautier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
        }
      }
    }
  }
})
