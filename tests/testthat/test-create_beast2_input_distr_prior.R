context("create_beast2_input_distr_prior")

test_that("RLN", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">",
    "    <distribution id=\"YuleModel.t:test_output_0\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@birthRate.t:test_output_0\">", # nolint XML
    "        <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>", # nolint this is no absolute path
    "    </prior>",
    "    <prior id=\"ucldStdevPrior.c:test_output_0\" name=\"distribution\" x=\"@ucldStdev.c:test_output_0\">", # nolint XML
    "        <Gamma id=\"Gamma.0\" name=\"distr\">",
    "            <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "            <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "        </Gamma>",
    "    </prior>",
    "</distribution>"

  )
  created <- create_beast2_input_distr_prior(
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
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})




test_that("Yule", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_aco\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_aco\" name=\"distribution\" x=\"@birthRate.t:anthus_aco\">", # nolint XML
    "        <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- create_beast2_input_distr_prior(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco")
    ),
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco")
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "anthus_aco",
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})
