library(beautier)

################################################################################
# Site models
################################################################################

brute_force_1_site_models <- function() {

  n_fail <- 0

  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {

        output_xml_filename <- tempfile()
        create_beast2_input_file(
          input_fasta_filenames = input_fasta_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          output_xml_filename = output_xml_filename
        )
        is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
        testthat::expect_true(is_ok)
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beautier:::is_beast2_input_file(output_xml_filename, verbose = TRUE)
          n_fail <- n_fail + 1
        }
      }
    }
  }
  n_fail
}

################################################################################
# Clock models
################################################################################

brute_force_1_clock_models_fixed_crown_age <- function() {

  n_fail <- 0

  clock_models <- beautier:::create_clock_models()
  testthat::expect_true(length(clock_models) > 1)

  for (clock_model in clock_models) {
    input_fasta_filename <- get_fasta_filename()
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      clock_models = clock_model,
      output_xml_filename = output_xml_filename,
      fixed_crown_ages = TRUE,
      initial_phylogenies = beautier::fasta_to_phylo(
        input_fasta_filename, crown_age = 15)
    )
    is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
    testthat::expect_true(is_ok)
    if (!is_ok) {
      print(paste(site_model$name, clock_model$name, tree_prior$name))
      beautier:::is_beast2_input_file(output_xml_filename, verbose = TRUE)
      n_fail <- n_fail + 1
    }
  }
  n_fail
}

################################################################################
# Tree priors
################################################################################

brute_force_1_tree_priors <- function() {

  n_fail <- 0

  tree_priors <- beautier:::create_tree_priors()
  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (tree_prior in tree_priors) {

    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      tree_priors = tree_prior,
      output_xml_filename = output_xml_filename
    )
    is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
    testthat::expect_true(is_ok)
    if (!is_ok) {
      print(paste(site_model$name, clock_model$name, tree_prior$name))
      beautier:::is_beast2_input_file(output_xml_filename, verbose = TRUE)
      n_fail <- n_fail + 1
    }
  }
  n_fail
}

brute_force_1_tree_priors_fixed_crown_age <- function() {

  n_fail <- 0

  tree_priors <- beautier:::create_tree_priors()
  input_fasta_filename <- beautier:::get_path("anthus_aco.fas")

  for (tree_prior in tree_priors) {
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      tree_priors = tree_prior,
      output_xml_filename = output_xml_filename,
      fixed_crown_ages = TRUE,
      initial_phylogenies = beautier::fasta_to_phylo(
        input_fasta_filename, crown_age = 15)
    )
    is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
    testthat::expect_true(is_ok)
    if (!is_ok) {
      print(paste(site_model$name, clock_model$name, tree_prior$name))
      beautier:::is_beast2_input_file(output_xml_filename, verbose = TRUE)
      n_fail <- n_fail + 1
    }
  }
  n_fail
}

################################################################################
# Combinations
################################################################################

brute_force_1_combinations_fixed_crown_age <- function() {

  n_fail <- 0

  input_fasta_filename <- beautier::get_path("anthus_aco.fas")

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {

        output_xml_filename <- tempfile()
        create_beast2_input_file(
          input_fasta_filenames = input_fasta_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          output_xml_filename = output_xml_filename,
          fixed_crown_ages = TRUE,
          initial_phylogenies = beautier::fasta_to_phylo(
            input_fasta_filename, crown_age = 15)
        )
        is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
        testthat::expect_true(is_ok)
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beautier:::is_beast2_input_file(output_xml_filename, verbose = TRUE)
          n_fail <- n_fail + 1
        }
      }
    }
  }
  n_fail
}

n_fail <- 0
n_fail <- n_fail + brute_force_1_site_models()
n_fail <- n_fail + brute_force_1_clock_models_fixed_crown_age()
n_fail <- n_fail + brute_force_1_tree_priors()
n_fail <- n_fail + brute_force_1_tree_priors_fixed_crown_age()
n_fail <- n_fail + brute_force_1_combinations_fixed_crown_age()
quit(status = n_fail, save = "no")
