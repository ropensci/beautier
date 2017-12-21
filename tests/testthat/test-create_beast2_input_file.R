context("create_beast2_input_file")

test_that("checks input", {

  # Don't: input is checked by 'create_beast2_input'
  # See 'create_beast2_input' tests
})

test_that("Create CCP posterior with random initial tree", {

  if (!is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 1,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_ccp_tree_prior()
  )
  testthat::expect_true(beastier::is_posterior(posterior))
})

test_that("Create BD posterior with random initial tree", {

  if (!is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 1,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_bd_tree_prior()
  )
  testthat::expect_true(beastier::is_posterior(posterior))

})

test_that("A fixed crown age must have equal TreeHeights", {

  if (!is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_bd_tree_prior(),
    fixed_crown_age = TRUE
  )
  testthat::expect_true(all(posterior$estimates$TreeHeight
    == posterior$estimates$TreeHeight[1]))
})


test_that(paste0("Fixed and specified crown age must result in a posterior ",
  "with that TreeHeight"), {

  if (!is_on_travis()) return()

  crown_age <- 123
  posterior <- beautier::create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc = create_mcmc(chain_length = 10000),
    fixed_crown_age = TRUE,
    crown_age = crown_age
  )
  testthat::expect_equal(posterior$estimates$TreeHeight[1], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(posterior$estimates$TreeHeight[10], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(crown_age,
    get_phylo_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
})

test_that("Can specify fixed crown age", {

  if (!is_on_travis()) return()

  input_fasta_filename <- beautier::get_fasta_filename()
  output_xml_filename_fixed <- tempfile()

  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_bd_tree_prior(),
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_age = TRUE,
    initial_phylogenies = beautier::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )
  testthat::expect_true(
    is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("Produce XML for Yule species tree prior", {

  if (!is_on_travis()) return()

  input_fasta_filename <- get_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_yule_tree_prior(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    is_beast2_input_file(output_xml_filename)
  )
})

test_that("strict clock model produce a valid BEAST2 input file", {

  if (!is_on_travis()) return()

  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = get_fasta_filename(),
    clock_models = create_strict_clock_model(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(is_beast2_input_file(output_xml_filename))
})

test_that("RLN clock model produce a valid BEAST2 input file", {

  if (!is_on_travis()) return()

  output_xml_filename <- tempfile() # nolint
  create_beast2_input_file(
    input_fasta_filenames = get_fasta_filename(),
    clock_models = create_rln_clock_model(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(is_beast2_input_file(output_xml_filename))
})
