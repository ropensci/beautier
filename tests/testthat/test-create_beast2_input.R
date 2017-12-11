context("create_beast2_input")
# Does
# * check the interface
# * check if XML created is valid with minimal tests
# Does not
# * check if valid XML files are reproduced.
#   'test-create_beast2_input_by_reproducingf_files.R' does that
# * check if XML created is valid with thorough tests.
#   'test-create_beast2_input_file.R' does that

################################################################################
# General
################################################################################
test_that("input is checked, one alignment", {

  testthat::expect_silent(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename()
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = "nonexisting" # Error
    ),
    "input_fasta_filenames not found"
  )
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "mcmc must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      tree_priors = "nonsense"
    ),
    "tree_priors must be valid, as returned by 'create_tree_priors'"
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      fixed_crown_age = "nonsense" # Error
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      site_models = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      tree_priors = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      clock_models = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      mcmc = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_fasta_filename(),
      initial_phylogenies = "nonsense"
    )
  )
})

test_that("input is checked, two alignments", {

  fasta_filename_1 <- beautier:::get_path("anthus_nd2.fas")
  fasta_filename_2 <- beautier:::get_path("anthus_aco.fas")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- beautier:::get_ids(input_fasta_filenames)

  # Two filesnames, one site model
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    )
  )

  # Two filesnames, one clock model
  if (1 == 2) {
    testthat::expect_silent(
      create_beast2_input(
        input_fasta_filenames = input_fasta_filenames,
        clock_models = create_strict_clock_models(ids = ids[1])
      )
    )
  }

  # Two filesnames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    )
  )

  # Two filesnames, one phylogeny
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})

test_that("Run all defaults", {

  created <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_fasta_filename()
  )

  testthat::expect_true(are_beast2_input_lines(created))
})

################################################################################
# Site models
################################################################################

################################################################################
# Site model: GTR
################################################################################

test_that("Run GTR", {

  created <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_fasta_filename(),
    site_models = create_gtr_site_model()
  )

  testthat::expect_true(are_beast2_input_lines(created))
})

################################################################################
# Site model: HKY
################################################################################

################################################################################
# Site model: JC69
################################################################################


################################################################################
# Site model: TN93
################################################################################


################################################################################
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("Use of a strict clock", {

  input_fasta_filename <- beautier::get_fasta_filename()
  id <- get_id(input_fasta_filename)
  lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    clock_models = create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(id = id)
    )
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

test_that("Use of a RLN clock", {

  lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_fasta_filename(),
    clock_models = create_rln_clock_model()
  )
  testthat::expect_true(are_beast2_input_lines(lines))

})

################################################################################
# Clock model: strict
################################################################################



################################################################################
# Tree priors
################################################################################

################################################################################
# Tree prior: BD
################################################################################

test_that("Run BD tree prior", {

  created <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_fasta_filename(),
    tree_priors = create_bd_tree_prior()
  )

  testthat::expect_true(are_beast2_input_lines(created))
})


################################################################################
# Tree prior: CBS
################################################################################

################################################################################
# Tree prior: CCP
################################################################################

################################################################################
# Tree prior: CEP
################################################################################

test_that("Run CEP", {

  lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_fasta_filename(),
    tree_priors = beautier::create_cep_tree_prior()
  )
  testthat::expect_true(are_beast2_input_lines(lines))

})

################################################################################
# Tree prior: Yule
################################################################################


################################################################################
# Priors
################################################################################

################################################################################
# Initial phylogenies
################################################################################

test_that("JC69 JC69 strict strict coalescent_exp_population", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_cep_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

test_that("TN93 TN93 strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_tn93_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})



test_that("GTR GTR strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_gtr_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})


test_that("GTR TN93 strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

test_that("JC69 JC69 strict relaxed_log_normal yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_rln_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

#-------------------------------------------------------------------------------
# Brute force tests, two alignments
#-------------------------------------------------------------------------------
test_that("All site models, clock models and tree priors, crown age est", {

  if (!is_on_travis()) return()

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)

  for (site_model_1 in beautier::create_site_models()) {
    for (site_model_2 in beautier::create_site_models()) {
      for (clock_model_1 in beautier::create_clock_models()) {
        for (clock_model_2 in beautier::create_clock_models()) {
          for (tree_prior in beautier::create_tree_priors()) {

            lines <- create_beast2_input(
              input_fasta_filenames = input_fasta_filenames,
              site_models = list(site_model_1, site_model_2),
              clock_models = list(clock_model_1, clock_model_2),
              tree_priors = list(tree_prior, tree_prior)
            )
            testthat::expect_true(are_beast2_input_lines(lines))
          }
        }
      }
    }
  }
})
