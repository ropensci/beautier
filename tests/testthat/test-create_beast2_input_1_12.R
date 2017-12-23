context("create_beast2_input_1_12")
# Does
# * check the interface
# * check if XML created is valid with minimal tests
# Does not
# * check if valid XML files are reproduced.
#   'test-create_beast2_input_1_12_by_reproducing_files.R' does that
# * check if XML created is valid with thorough tests.
#   'test-create_beast2_input_1_12_file.R' does that

################################################################################
# General
################################################################################
test_that("input is checked, one alignment", {

  testthat::expect_silent(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename()
    )
  )

  # 1 input_fasta_filenames,
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = "nonexisting" # Error
    ),
    "'input_fasta_filenames' must be the name of one or more present files"
  )

  # 2 site_models
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      site_models = "nonsense"
    ),
    paste0(
      "'site_models' must be a valid site model, ",
      "or a list of valid site models, ",
      "as returned by 'create_site_model'"
    )
  )

  # 3 clock_models
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      clock_models = "nonsense"
    ),
    paste0(
      "'clock_models' must be a valid clock model, ",
      "or a list of valid clock models, ",
      "as returned by 'create_clock_model'"
    )
  )

  # 4 tree_priors
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      tree_priors = "nonsense"
    ),
    paste0(
      "'tree_priors' must be a valid tree prior, ",
      "or a list of valid tree priors, ",
      "as returned by 'create_tree_prior'"
    )
  )

  # 5 mcmc
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "'mcmc' must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  # 6 misc_options
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      misc_options = "nonsense"
    ),
    "'misc_options' must be a valid misc options object"
  )

  # 7 fixed_crown_ages
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      fixed_crown_ages = "nonsense" # Error
    ),
    "'fixed_crown_ages' must be one or more booleans"
  )

  # 8 initial_phylogenies
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = get_fasta_filename(),
      initial_phylogenies = "nonsense"
    )
  )
})

test_that("input is checked, two alignments", {

  input_fasta_filenames <- beautier:::get_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- beautier:::get_ids(input_fasta_filenames)

  # Two filesnames, one site model
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    ),
    "Must supply as much input_fasta_filenames as site_models"
  )

  # Two filesnames, one clock model
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      clock_models = create_strict_clock_models(ids = ids[1])
    ),
    "Must supply as much input_fasta_filenames as clock_models"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    ),
    "Must supply as much input_fasta_filenames as tree priors"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      fixed_crown_ages = TRUE
    ),
    "Must supply as much input_fasta_filenames as fixed crown ages"
  )

  # Two filesnames, one phylogeny
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      initial_phylogenies = c(ape::rcoal(4))
    ),
    "Must supply as much input_fasta_filenames as initial_phylogenies"
  )


  # Two filesnames, two RLN clock models
  testthat::expect_error(
    create_beast2_input_1_12(
      input_fasta_filenames = input_fasta_filenames,
      clock_models = list(
        create_rln_clock_model(id = ids[1]),
        create_rln_clock_model(id = ids[1])
      )
    ),
    "Cannot have shared Relaxed Log-Normal clock models"
  )
})
