context("create_beast2_input")

test_that("use", {

  expect_silent(
    create_beast2_input(
      input_filename = get_fasta_filename()
    )
  )

})

test_that("use with calibration node", {

  fasta_filename <- get_fasta_filename()
  expect_silent(
    create_beast2_input(
      input_filename = fasta_filename,
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      )
    )
  )

})

test_that("Run MRCA, need one branchRateModel, beautier issue #26", {

  fasta_filename <- get_fasta_filename()
  lines <- create_beast2_input(
    input_filename = fasta_filename,
    site_model = create_jc69_site_model(),
    clock_model = create_rln_clock_model(),
    tree_prior = create_cep_tree_prior(),
    mrca_prior = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = TRUE
    )
  )
  expect_equal(
    1,
    sum(grepl(x = lines, pattern = " *<branchRateModel.*"))
  )
})

test_that("Run with default MRCA, beautier issue #75", {

  expect_silent(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      site_model = create_jc69_site_model(),
      clock_model = create_rln_clock_model(),
      tree_prior = create_cep_tree_prior(),
      mrca_prior = create_mrca_prior()
    )
  )
})

test_that("abuse: one alignment", {

  expect_silent(
    create_beast2_input(
      input_filename = get_fasta_filename()
    )
  )

  # 1 input_filename
  expect_error(
    create_beast2_input(
      input_filename = "nonexisting" # Error
    ),
    "'input_filename' not found"
  )
  # input_filenames
  expect_error(
    create_beast2_input(
      input_filenames = "nonexisting" # Error
    ),
    "'input_filenames' is deprecated, use 'input_filename' instead"
  )

  # 2 site_model
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      site_model = "nonsense"
    ),
    "'site_model' must be a valid site model"
  )
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      site_models = "nonsense"
    ),
    "'site_models' is deprecated, use 'site_model' instead"
  )

  # 3 clock_models
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      clock_model = "nonsense"
    ),
    paste0(
      "'clock_model' must be a valid clock model, ",
      "as returned by 'create_clock_model'"
    )
  )
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      clock_models = "nonsense"
    ),
    "'clock_models' is deprecated, use 'clock_model' instead"
  )

  # 4 tree_prior
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      tree_prior = "nonsense"
    ),
    paste0(
      "'tree_prior' must be a valid tree prior, ",
      "as returned by 'create_tree_prior'"
    )
  )
  # tree_priors
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      tree_priors = "nonsense"
    ),
    "'tree_priors' is deprecated, use 'tree_prior' instead"
  )

  # mrca_prior
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mrca_prior = "nonsense"
    ),
    "'mrca_prior' must be NA or a valid mrca object"
  )
  # mrca_priors
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mrca_priors = "nonsense"
    ),
    "'mrca_priors' is deprecated, use 'mrca_prior' instead."
  )

  # 5 mcmc
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "'mcmc' must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  # 6 beauti_options
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      beauti_options = "nonsense"
    ),
    "'beauti_options' must be a valid misc options object"
  )

  # 7 posterior_crown_age
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      posterior_crown_age = "nonsense" # Error
    ),
    "'posterior_crown_age' is deprecated"
  )

  # Higher-level abuse
  expect_error(
    create_beast2_input(
      input_filename = get_beautier_path("anthus_aco_sub.fas"),
      tree_prior = create_cbs_tree_prior(group_sizes_dimension = 5)
    ),
    "'group_sizes_dimension' \\(5\\) must be less than the number of taxa \\(5\\)" # nolint
  )

  fasta_filename <- get_fasta_filename()
  expect_error(
    create_beast2_input(
      input_filename = fasta_filename,
      mrca_prior = create_mrca_prior(
        alignment_id = paste0("broken_", get_alignment_id(fasta_filename)),
        taxa_names = get_taxa_names(fasta_filename)
      )
    ),
    "All MRCA prior's alignment IDs must match the FASTA file IDs"
  )

  expect_error(
    create_beast2_input(
      input_filename = fasta_filename,
      mrca_prior = create_mrca_prior(
        taxa_names = paste0("broken_", get_taxa_names(fasta_filename))
      )
    ),
    "All MRCA prior's taxa names must be FASTA file taxa names"
  )

  # Detect intersecting monophyletic MRCA priors
  fasta_filename <- get_beautier_path("test_output_0.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)
  prior_one_two <- create_mrca_prior(
    taxa_names = all_taxa_names[1:2],
    is_monophyletic = TRUE
  )
  prior_two_three <- create_mrca_prior(
    taxa_names = all_taxa_names[2:3],
    is_monophyletic = TRUE
  )

  intersecting_mrca_priors <- list(prior_one_two, prior_two_three)

  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mrca_prior = intersecting_mrca_priors
    ),
    "Monophyletic MRCA priors must have taxon sets without intersection"
  )
})

test_that("abuse: two alignments", {

  input_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- get_alignment_ids(input_filenames)

  # Two filenames, one site model
  expect_error(
    create_beast2_input(
      input_filename = input_filenames
    ),
    "Must use one alignment, site model, clock model and tree prior"
  )
})
