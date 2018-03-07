context("create_beast2_input")

# These tests check if there is output produced without warnings or errors
#
# The output is not checked here.
# This *is* done at:
#  1) babette 'run_beast_2' tests
#  2) scripts/run_..
# as these need beastier to validate if the BEAST2 XML files are valid

################################################################################
# Defaults
################################################################################

test_that("Run all defaults", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename()
    )
  )
})

################################################################################
# Site models
################################################################################

test_that("Run GTR", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      site_models = create_gtr_site_model()
    )
  )
})

test_that("Run HKY", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      site_models = create_hky_site_model()
    )
  )
})

test_that("Run JC69", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      site_models = create_jc69_site_model()
    )
  )
})

test_that("Run TN93", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      site_models = create_tn93_site_model()
    )
  )
})

################################################################################
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("Use of a strict clock", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      clock_models = create_strict_clock_model()
    )
  )
})

test_that("Use of a RLN clock", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      clock_models = create_rln_clock_model()
    )
  )
})

################################################################################
# Tree priors
################################################################################
test_that("Run BD tree prior", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = create_bd_tree_prior()
    )
  )
})

test_that("Run CBS tree prior", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = create_cbs_tree_prior()
    )
  )
})

test_that("Run CCP tree prior", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = create_ccp_tree_prior()
    )
  )
})

test_that("Run CEP tree prior", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = create_cep_tree_prior()
    )
  )
})

test_that("Run Yule tree prior", {
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = create_yule_tree_prior()
    )
  )
})

################################################################################
# MRCA priors
################################################################################

test_that("Run MRCA, no distr", {
  fasta_filename <- get_fasta_filename()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = fasta_filename,
      mrca_priors = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      )
    )
  )
})

test_that("Run MRCA, MRCA distr", {
  skip("WIP")
  fasta_filename <- get_fasta_filename()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = fasta_filename,
      mrca_priors = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      )
    )
  )
})


################################################################################
# Initial phylogenies
################################################################################

test_that("JC69 JC69 strict strict coalescent_exp_population", {

  input_fasta_filename_1 <- get_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_cep_tree_prior()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = list(site_model_1, site_model_2),
      clock_models = list(clock_model_1, clock_model_2),
      tree_priors = list(tree_prior, tree_prior)
    )
  )
})

test_that("TN93 TN93 strict strict yule", {

  input_fasta_filename_1 <- get_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_tn93_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = list(site_model_1, site_model_2),
      clock_models = list(clock_model_1, clock_model_2),
      tree_priors = list(tree_prior, tree_prior)
    )
  )
})



test_that("GTR GTR strict strict yule", {

  input_fasta_filename_1 <- get_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_gtr_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = list(site_model_1, site_model_2),
      clock_models = list(clock_model_1, clock_model_2),
      tree_priors = list(tree_prior, tree_prior)
    )
  )
})


test_that("GTR TN93 strict strict yule", {

  input_fasta_filename_1 <- get_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = list(site_model_1, site_model_2),
      clock_models = list(clock_model_1, clock_model_2),
      tree_priors = list(tree_prior, tree_prior)
    )
  )
})

test_that("JC69 JC69 strict relaxed_log_normal Yule", {

  input_filenames <- get_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_rln_clock_model()
  tree_prior <- create_yule_tree_prior()
  testthat::expect_silent(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = list(site_model_1, site_model_2),
      clock_models = list(clock_model_1, clock_model_2),
      tree_priors = list(tree_prior, tree_prior)
    )
  )
})
