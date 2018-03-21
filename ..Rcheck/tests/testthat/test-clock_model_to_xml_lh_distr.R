context("clock_model_to_xml_lh_distr")

################################################################################
# One alignment
################################################################################

test_that("strict", {

  expected <- c(
    "<branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "    <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "</branchRateModel>"
  )
  created <- beautier:::clock_model_to_xml_lh_distr(
    create_strict_clock_model(
      id = "test_output_0"
    )
  )
  testit::assert(beautier:::is_xml(expected))
  testit::assert(beautier:::is_xml(created))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

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
  created <- beautier:::clock_model_to_xml_lh_distr(
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
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})


################################################################################
# Two alignment, unlinked clock models
################################################################################
