context("create_beast2_input_1_12")

test_that("abuse: one alignment", {

  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename()
    )
  )

  # Convert one site model to list for user
  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      site_models = create_jc69_site_model()
    )
  )

  # Convert one clock model to list for user
  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      clock_models = create_strict_clock_model()
    )
  )

  # Convert one tree prior to list for user
  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      tree_priors = create_yule_tree_prior()
    )
  )

  # Create one phylogeny to list for user
  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      initial_phylogenies = fasta_to_phylo(
        fasta_filename = get_fasta_filename(),
        crown_age = 10
      )
    )
  )

})

test_that("abuse: one alignment", {

  testthat::expect_silent(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename()
    )
  )

  # 1 input_filenames,
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = "nonexisting" # Error
    ),
    "'input_filenames' must be the name of one or more present files"
  )

  # 2 site_models
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
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
      input_filenames = get_fasta_filename(),
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
      input_filenames = get_fasta_filename(),
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
      input_filenames = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "'mcmc' must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  # 6 beauti_options
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      beauti_options = "nonsense"
    ),
    "'beauti_options' must be a valid misc options object"
  )

  # 7 fixed_crown_ages
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      fixed_crown_ages = "nonsense" # Error
    ),
    "'fixed_crown_ages' must be one or more booleans"
  )

  # 8 initial_phylogenies
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = get_fasta_filename(),
      initial_phylogenies = "nonsense"
    )
  )
})
