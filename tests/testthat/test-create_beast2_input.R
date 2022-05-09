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
  # 2 site_model
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      site_model = "nonsense"
    ),
    "'site_model' must be a valid site model"
  )
  # 3 clock_models
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      clock_model = "nonsense"
    ),
    "'clock_model' must be a valid clock model"
  )
  # 4 tree_prior
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      tree_prior = "nonsense"
    ),
    "'tree_prior' must be a valid tree prior"
  )
  # mrca_prior
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mrca_prior = "nonsense"
    ),
    "'mrca_prior' must be a valid MRCA prior"
  )
  # 5 mcmc, tested in-depth by 'check_mcmc'
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      mcmc = "nonsense"
    ),
    "mcmc"
  )

  # 6 beauti_options
  expect_error(
    create_beast2_input(
      input_filename = get_fasta_filename(),
      beauti_options = "nonsense"
    ),
    "'beauti_options' must be a valid BEAUti options"
  )
  # Higher-level abuse
  # Tested by 'check_file_and_model_agree'
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

test_that("Issue https://github.com/ropensci/beastier/issues/63", {
  xml_text <- create_beast2_input(
    beautier::get_fasta_filename(),
    site_model = create_gtr_site_model(gamma_site_model = create_gamma_site_model()),
    mcmc = create_mcmc(
      chain_length = 9e+07,
      store_every = -1,
      pre_burnin = 0,
      n_init_attempts = 10,
      sample_from_prior = FALSE,
      tracelog = create_tracelog(filename = "Step 4.2 COI BEAST output.log", log_every = 9000),
      screenlog = create_screenlog(log_every = 9000),
      treelog = create_treelog(filename = "Step 4.2 COI BEAST output.trees",log_every = 9000)),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(
    1,
    length(
      stringr::str_subset(
        xml_text,
        "logger id=.treelog.t:test_output_.*fileName=\"Step.*"
      )
    )
  )
})
