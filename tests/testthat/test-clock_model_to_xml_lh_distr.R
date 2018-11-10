context("clock_model_to_xml_lh_distr")

test_that("strict", {

  expected <- c(
    "<branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "    <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  created <- clock_model_to_xml_lh_distr(
    create_strict_clock_model(
      id = "test_output_0"
    )
  )
  testit::assert(is_xml(expected))
  testit::assert(is_xml(created))
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      n_rate_categories = 0
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      n_rate_categories = 1
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      n_rate_categories = 2
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      n_rate_categories = -1
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      mean_clock_rate = "1.1",
      n_rate_categories = -1
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      normalize_mean_clock_rate = TRUE
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))

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
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1,
      normalize_mean_clock_rate = FALSE
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("RLN + MRCA with distr", {

  # From 'rln_mrca_one_div_x_2_5.xml'
  expected <- c(
    "<branchRateModel id=\"RelaxedClock.c:anthus_aco_sub\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" clock.rate=\"@ucldMean.c:anthus_aco_sub\" rateCategories=\"@rateCategories.c:anthus_aco_sub\" tree=\"@Tree.t:anthus_aco_sub\">",
    "    <LogNormal id=\"LogNormalDistributionModel.c:anthus_aco_sub\" S=\"@ucldStdev.c:anthus_aco_sub\" meanInRealSpace=\"true\" name=\"distr\">",
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>",
    "    </LogNormal>",
    "</branchRateModel>"
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  created <- clock_model_to_xml_lh_distr(
    create_rln_clock_model(
      id = "anthus_aco_sub",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1
    ),
    mrca_priors = list(
      create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
