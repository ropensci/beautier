context("create_beast2_input_file")

test_that("checks input", {

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = "nonexisting", # Error
      mcmc = create_mcmc(chain_length = 10000),
      output_xml_filename = "output.xml"
    )
  )
  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc = list(chain_length = 0), # Error
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc = create_mcmc(chain_length = 10000),
      tree_priors = create_tree_prior(name = "nonsense"),
      output_xml_filename = "output.xml"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc = create_mcmc(chain_length = 10000),
      output_xml_filename = "output.xml",
      fixed_crown_age = "nonsense" # Error
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      output_xml_filename = "output.xml",
      site_models = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      output_xml_filename = "output.xml",
      tree_priors = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      output_xml_filename = "output.xml",
      clock_models = "nonsense"
    )
  )

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")

  testthat::expect_error(
    create_beast2_input_file(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      output_xml_filename = "output.xml",
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})

test_that("Create CCP posterior with random initial tree", {

  if (!beautier::is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 1,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_ccp_tree_prior()
  )
  testthat::expect_true(RBeast::is_posterior(posterior))
})

test_that("Create BD posterior with random initial tree", {

  if (!beautier::is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 1,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_bd_tree_prior()
  )
  testthat::expect_true(RBeast::is_posterior(posterior))

})

test_that("A fixed crown age must have equal TreeHeights", {

  if (!beautier::is_on_travis()) return()

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

  if (!beautier::is_on_travis()) return()

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
    beautier::get_phylogeny_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
})

test_that("Can specify fixed crown age", {

  if (!beautier::is_on_travis()) return()

  input_fasta_filename <- beautier::get_input_fasta_filename()
  output_xml_filename_fixed <- tempfile()

  # Input file must be found
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_bd_tree_prior(),
    output_xml_filename = output_xml_filename_fixed,
    fixed_crown_age = TRUE,
    initial_phylogenies = beautier::fasta_to_phylo(
      input_fasta_filename, crown_age = 15)
  )
  testthat::expect_true(
    beautier::is_beast2_input_file(output_xml_filename_fixed)
  )
})

test_that("Produce XML for Yule species tree prior", {

  if (!beautier::is_on_travis()) return()

  input_fasta_filename <- get_input_fasta_filename()
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    tree_priors = create_yule_tree_prior(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(
    beautier::is_beast2_input_file(output_xml_filename)
  )
})


test_that("All site models produce a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  site_models <- beautier::create_site_models()
  testthat::expect_true(length(site_models) > 1)
  for (site_model in site_models) {

    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
      site_models = site_model,
      output_xml_filename = output_xml_filename
    )
    testthat::expect_true(
      beautier::is_beast2_input_file(output_xml_filename)
    )
  }
})

test_that(paste0("All site models produce a valid BEAST2 input file, ",
  "fixed crown age"), {

  if (!beautier::is_on_travis()) return()

  site_models <- beautier::create_site_models()
  testthat::expect_true(length(site_models) > 1)
  for (site_model in site_models) {

    input_fasta_filename <- get_input_fasta_filename()
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = input_fasta_filename,
      site_models = site_model,
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

test_that("strict clock model produce a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_fasta_filenames = get_input_fasta_filename(),
    clock_models = create_strict_clock_model(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(beautier::is_beast2_input_file(output_xml_filename))
})

test_that("RLN clock model produce a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  #output_xml_filename <- tempfile()
  output_xml_filename <- "~/fix.xml"
  create_beast2_input_file(
    input_fasta_filenames = get_input_fasta_filename(),
    clock_models = create_rln_clock_model(),
    output_xml_filename = output_xml_filename
  )
  testthat::expect_true(beautier::is_beast2_input_file(output_xml_filename))
})


test_that(paste0("All clock models produce a valid BEAST2 input file, ",
  "fixed crown age"), {

  if (!beautier::is_on_travis()) return()

  clock_models <- beautier::create_clock_models()
  testthat::expect_true(length(clock_models) > 1)

  for (clock_model in clock_models) {
    input_fasta_filename <- get_input_fasta_filename()
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

test_that("All tree priors produce a valid BEAST2 input file", {

  if (!beautier::is_on_travis()) return()

  skip("WIP")

  tree_priors <- beautier::create_tree_priors()
  testthat::expect_true(length(tree_priors) > 1)

  for (tree_prior in tree_priors) {
    if (is_cbs_tree_prior(tree_prior)) {
      # CBS fails, because the groupSize's dimension is 5 by default,
      # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
      # groupSize cannot be more than 4
      next
    }
    output_xml_filename <- tempfile()
    create_beast2_input_file(
      input_fasta_filenames = get_input_fasta_filename(),
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

  skip("WIP")

  tree_priors <- beautier::create_tree_priors()
  testthat::expect_true(length(tree_priors) > 1)

  for (tree_prior in tree_priors) {
    if (is_cbs_tree_prior(tree_prior)) {
      # CBS fails, because the groupSize's dimension is 5 by default,
      # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
      # groupSize cannot be more than 4
      next
    }
    input_fasta_filename <- get_input_fasta_filename()
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
      print(tree_prior)
      beautier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    }
    testthat::expect_true(ok)
  }
})
