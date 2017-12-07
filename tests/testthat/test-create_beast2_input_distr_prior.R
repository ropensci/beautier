context("create_beast2_input_distr_prior")

test_that("RLN", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">",
    "    <distribution id=\"YuleModel.t:test_output_0\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>",
    "    <prior id=\"YuleBirthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@birthRate.t:test_output_0\">",
    "        <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>",
    "    </prior>",
    "    <prior id=\"ucldStdevPrior.c:test_output_0\" name=\"distribution\" x=\"@ucldStdev.c:test_output_0\">",
    "        <Gamma id=\"Gamma.0\" name=\"distr\">",
    "            <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"alpha\">0.5396</parameter>",
    "            <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"beta\">0.3819</parameter>",
    "        </Gamma>",
    "    </prior>",
    "</distribution>"

  )
  created <- beautier:::create_beast2_input_distr_prior(
    site_models = list(
      create_jc69_site_model(id = "test_output_0")
    ),
    clock_models = list(
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
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "test_output_0",
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
