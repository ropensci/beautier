context("create_beast2_input")
# Does
# * check the interface
# * check if XML created is valid with minimal tests
# Does not
# * check if valid XML files are reproduced.
#   'test-create_beast2_input_by_reproducing_files.R' does that
# * check if XML created is valid with thorough tests.
#   'test-create_beast2_input_file.R' does that

test_that("use", {

  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename()
    )
  )

})

test_that("use with fixed crown age", {

  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      posterior_crown_age = 15
    )
  )

})

test_that("use with calibration node", {

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

test_that("Run MRCA, need one branchRateModel, beautier issue #26", {

  fasta_filename <- get_fasta_filename()
  lines <- create_beast2_input(
    input_filenames = fasta_filename,
    site_models = create_jc69_site_model(),
    clock_models = create_rln_clock_model(),
    tree_priors = create_cep_tree_prior(),
    mrca_priors = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = TRUE
    )
  )
  testthat::expect_equal(
    1,
    sum(grepl(x = lines, pattern = " *<branchRateModel.*"))
  )
})

test_that("abuse: one alignment", {

  testthat::expect_silent(
    create_beast2_input(
      input_filenames = get_fasta_filename()
    )
  )

  # 1 input_filenames,
  testthat::expect_error(
    create_beast2_input(
      input_filenames = "nonexisting" # Error
    ),
    "'input_filenames' must be the name of one or more present files"
  )

  # 2 site_models
  testthat::expect_error(
    create_beast2_input(
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
    create_beast2_input(
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
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      tree_priors = "nonsense"
    ),
    paste0(
      "'tree_priors' must be a valid tree prior, ",
      "or a list of valid tree priors, ",
      "as returned by 'create_tree_prior'"
    )
  )

  # mrca_priors
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      mrca_priors = "nonsense"
    ),
    "'mrca_priors' must be NA or a valid mrca object"
  )

  # 5 mcmc
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "'mcmc' must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  # 6 misc_options
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      misc_options = "nonsense"
    ),
    "'misc_options' must be a valid misc options object"
  )

  # 7 posterior_crown_age
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      posterior_crown_age = "nonsense" # Error
    ),
    "'posterior_crown_age' must be either NA or a non-zero postive value"
  )
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      posterior_crown_age = -12.34 # Error
    ),
    "'posterior_crown_age' must be either NA or a non-zero postive value"
  )

  # Higher-level abuse
  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_beautier_path("anthus_aco_sub.fas"),
      tree_priors = create_cbs_tree_prior(group_sizes_dimension = 5)
    ),
    "'group_sizes_dimension' \\(5\\) must be less than the number of taxa \\(5\\)" # nolint
  )

  fasta_filename <- get_fasta_filename()
  testthat::expect_error(
    create_beast2_input(
      input_filenames = fasta_filename,
      mrca_priors = create_mrca_prior(
        alignment_id = paste0("broken_", get_alignment_id(fasta_filename)),
        taxa_names = get_taxa_names(fasta_filename)
      )
    ),
    "All MRCA prior's alignment IDs must match the FASTA file IDs"
  )

  testthat::expect_error(
    create_beast2_input(
      input_filenames = fasta_filename,
      mrca_priors = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = paste0("broken_", get_taxa_names(fasta_filename))
      )
    ),
    "All MRCA prior's taxa names must be FASTA file taxa names"
  )

  # Detect intersecting monophyletic MRCA priors
  fasta_filename <- get_beautier_path("test_output_0.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)
  prior_one_two <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[1:2],
    is_monophyletic = TRUE
  )
  prior_two_three <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[2:3],
    is_monophyletic = TRUE
  )

  intersecting_mrca_priors <- list(prior_one_two, prior_two_three)

  testthat::expect_error(
    create_beast2_input(
      input_filenames = get_fasta_filename(),
      mrca_priors = intersecting_mrca_priors
    ),
    "Monophyletic MRCA priors must have taxon sets without intersection"
  )
})

test_that("abuse: two alignments", {

  input_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- beautier:::get_ids(input_filenames)

  # Two filesnames, one site model
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    ),
    "Must supply as much input_filenames as site_models"
  )

  # Two filesnames, one clock model
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      clock_models = create_strict_clock_models(ids = ids[1])
    ),
    "Must supply as much input_filenames as clock_models"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    ),
    "Must supply as much input_filenames as tree priors"
  )

  # Two filesnames, two RLN clock models
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      clock_models = list(
        create_rln_clock_model(id = ids[1]),
        create_rln_clock_model(id = ids[1])
      )
    ),
    "Cannot have shared Relaxed Log-Normal clock models"
  )
})
