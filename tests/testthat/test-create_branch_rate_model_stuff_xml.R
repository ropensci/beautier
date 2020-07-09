test_that("section of anthus_aco_sub_calibration.xml", {
  expected <- c(
    "<branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\" clock.rate=\"@clockRate.c:anthus_aco_sub\"/>" # nolint long line indeed
  )
  # This XML file has a normal distribution for the MRCA prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mcmc = create_mcmc(chain_length = 10000),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_normal_distr(
          id = 0,
          mean = create_mean_param(id = 1, value = "0.02"),
          sigma = create_sigma_param(id = 2, value = "0.001")
        ),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      ),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )

  created <- create_branch_rate_model_stuff_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("adapted from anthus_aco_sub_calibrated_no_prior.xml", {
  expected <- c(
    "<branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint indeed a long line
    "    <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint indeed a long line
    "</branchRateModel>"
  )

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mcmc = create_mcmc(chain_length = 10000),
      mrca_prior = create_mrca_prior(
        name = "every",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        clock_prior_distr_id = 0
      ),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )
  created <- create_branch_rate_model_stuff_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("Adapted from anthus_aco_sub_20181016_all.xml", {
  expected <- c(
    "<branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint long line indeed
    "    <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint long line indeed
    "</branchRateModel>"
  )
  fasta_filename <- "anthus_aco_sub.fas"
  inference_model <- init_inference_model(
    input_filename = get_beautier_path(fasta_filename),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(get_beautier_path(fasta_filename)),
        taxa_names = get_taxa_names(get_beautier_path(fasta_filename)),
        is_monophyletic = FALSE
      ),
      beauti_options = create_beauti_options(
        nucleotides_uppercase = TRUE,
        beast2_version = "2.5",
        required = "BEAST v2.5.0"
      )
    )
  )
  created <- create_branch_rate_model_stuff_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("Adapted from anthus_aco_sub_20181016_all_monophyletic.xml", {

  expected <- c(
    "<branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint long line indeed
    "    <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint long line indeed
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")), # nolint long line indeed
        taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
        is_monophyletic = TRUE
      ),
      beauti_options = create_beauti_options(
        nucleotides_uppercase = TRUE,
        beast2_version = "2.5",
        required = "BEAST v2.5.0"
      )
    )
  )
  created <- create_branch_rate_model_stuff_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})
