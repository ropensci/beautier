test_that("strict", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">", # nolint XML
    "            <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>",
    "        <branchRateModel id=\"StrictClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">", # nolint XML
    "            <parameter id=\"clockRate.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>",
    "    </distribution>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_beast2_input_distr_lh(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})


test_that("RLN", {

  expected <- c(
    "<distribution id=\"likelihood\" spec=\"util.CompoundDistribution\" useThreads=\"true\">", # nolint XML
    "    <distribution id=\"treeLikelihood.test_output_0\" spec=\"ThreadedTreeLikelihood\" data=\"@test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "        <siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "            <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint XML
    "            <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint XML
    "            <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint XML
    "            <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>", # nolint XML
    "        </siteModel>",
    "        <branchRateModel id=\"RelaxedClock.c:test_output_0\" spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" rateCategories=\"@rateCategories.c:test_output_0\" tree=\"@Tree.t:test_output_0\">", # nolint XML
    "            <LogNormal id=\"LogNormalDistributionModel.c:test_output_0\" S=\"@ucldStdev.c:test_output_0\" meanInRealSpace=\"true\" name=\"distr\">", # nolint XML
    "                <parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>", # nolint XML
    "            </LogNormal>",
    "            <parameter id=\"ucldMean.c:test_output_0\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>", # nolint XML
    "        </branchRateModel>",
    "    </distribution>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
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
  created <- create_beast2_input_distr_lh(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})
