test_that("strict", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_strict_clock_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expected <- c(
    "<branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "    <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})


test_that("RLN", {


  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN -1 rates", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN 0 rates", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" numberOfDiscreteRates=\"0\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        n_rate_categories = 0
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN 1 rates", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" numberOfDiscreteRates=\"1\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        id = "test_output_0",
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        n_rate_categories = 1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN 2 rates", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" numberOfDiscreteRates=\"2\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        n_rate_categories = 2
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})


test_that("RLN clock rate 1.0", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        n_rate_categories = -1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN clock rate 1.1", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.1</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        mean_clock_rate = "1.1",
        n_rate_categories = -1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN normalize", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" normalize=\"true\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        normalize_mean_clock_rate = TRUE
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN no normalize", {

  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "    <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1,
        normalize_mean_clock_rate = FALSE
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN + MRCA with distr", {

  # From 'rln_mrca_one_div_x_2_5.xml'
  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_aco_sub\" rateCategories=\"@rateCategories.c:anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">", # nolint XML
    "    <LogNormal id=\"LogNormalDistributionModel.c:anthus_aco_sub\" S=\"@ucldStdev.c:anthus_aco_sub\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "    </LogNormal>",
    "</branchRateModel>"
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("section of anthus_aco_sub_calibration.xml", {
  skip("WIP")
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

  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("adapted from anthus_aco_sub_calibrated_no_prior.xml", {
  skip("WIP")
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
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("Adapted from anthus_aco_sub_20181016_all.xml", {
  skip("WIP")
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
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("Adapted from anthus_aco_sub_20181016_all_monophyletic.xml", {

  skip("WIP")
  expected <- c(
    "<branchRateModel id=\"StrictClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">",
    "    <parameter id=\"clockRate.c:anthus_aco_sub\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>",
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
        alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
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
  created <- create_branch_rate_model_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})
