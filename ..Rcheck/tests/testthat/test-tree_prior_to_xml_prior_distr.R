context("tree_prior_to_xml_prior_distr")

test_that("BD", {

  expected <- c(
    "<distribution id=\"BirthDeath.t:test_output_0\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" birthDiffRate=\"@BDBirthRate.t:test_output_0\" relativeDeathRate=\"@BDDeathRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>", # nolint XML
    "<prior id=\"BirthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@BDBirthRate.t:test_output_0\">", # nolint XML
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"1000.0\"/>",
    "</prior>",
    "<prior id=\"DeathRatePrior.t:test_output_0\" name=\"distribution\" x=\"@BDDeathRate.t:test_output_0\">", # nolint XML
    "    <Uniform id=\"Uniform.4\" name=\"distr\"/>",
    "</prior>"
  )
  created <- beautier:::tree_prior_to_xml_prior_distr(
    tree_prior = create_bd_tree_prior(
      id = "test_output_0",
      birth_rate_distr = create_uniform_distr(id = 3, upper = "1000.0"),
      death_rate_distr = create_uniform_distr(id = 4, upper = NA)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CBS", {

  expected <- c(
    "<distribution id=\"BayesianSkyline.t:anthus_aco\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:anthus_aco\" popSizes=\"@bPopSizes.t:anthus_aco\">", # nolint XML is long
    "    <treeIntervals id=\"BSPTreeIntervals.t:anthus_aco\" spec=\"TreeIntervals\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML is long
    "</distribution>",
    "<distribution id=\"MarkovChainedPopSizes.t:anthus_aco\" spec=\"beast.math.distributions.MarkovChainDistribution\" jeffreys=\"true\" parameter=\"@bPopSizes.t:anthus_aco\"/>" # nolint XML is long
  )
  created <- beautier:::tree_prior_to_xml_prior_distr(
    tree_prior = create_cbs_tree_prior(id = "anthus_aco")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("CCP", {

  expected <- c(
    "<distribution id=\"CoalescentConstant.t:test_output_0\" spec=\"Coalescent\">", # nolint XML
    "    <populationModel id=\"ConstantPopulation.t:test_output_0\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:test_output_0\"/>", # nolint XML
    "    <treeIntervals id=\"TreeIntervals.t:test_output_0\" spec=\"TreeIntervals\" tree=\"@Tree.t:test_output_0\"/>", # nolint XML
    "</distribution>", # nolint XML
    "<prior id=\"PopSizePrior.t:test_output_0\" name=\"distribution\" x=\"@popSize.t:test_output_0\">", # nolint XML
    "    <OneOnX id=\"OneOnX.1\" name=\"distr\"/>",
    "</prior>"
  )
  created <- beautier:::tree_prior_to_xml_prior_distr(
    tree_prior = create_ccp_tree_prior(
      id = "test_output_0",
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CEP", {

  expected <- c(
    "<distribution id=\"CoalescentExponential.t:test_output_0\" spec=\"Coalescent\">", # nolint XML
    "    <populationModel id=\"ExponentialGrowth.t:test_output_0\" spec=\"ExponentialGrowth\" growthRate=\"@growthRate.t:test_output_0\" popSize=\"@ePopSize.t:test_output_0\"/>", # nolint XML
    "    <treeIntervals id=\"TreeIntervals.t:test_output_0\" spec=\"TreeIntervals\" tree=\"@Tree.t:test_output_0\"/>", # nolint XML
    "</distribution>",
    "<prior id=\"ePopSizePrior.t:test_output_0\" name=\"distribution\" x=\"@ePopSize.t:test_output_0\">", # nolint XML
    "    <OneOnX id=\"OneOnX.1\" name=\"distr\"/>",
    "</prior>",
    "<prior id=\"GrowthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@growthRate.t:test_output_0\">", # nolint XML
    "    <LaplaceDistribution id=\"LaplaceDistribution.0\" name=\"distr\">",
    "        <parameter id=\"RealParameter.1\" estimate=\"false\" name=\"mu\">0.001</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"scale\">30.701135</parameter>", # nolint XML
    "    </LaplaceDistribution>",
    "</prior>"
  )
  created <- beautier:::tree_prior_to_xml_prior_distr(
    tree_prior = create_cep_tree_prior(
      id = "test_output_0",
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("Yule", {

  expected <- c(
    "<distribution id=\"YuleModel.t:test_output_0\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>", # nolint XML
    "<prior id=\"YuleBirthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@birthRate.t:test_output_0\">", # nolint XML
    "    <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>"
  )
  created <- beautier:::tree_prior_to_xml_prior_distr(
    tree_prior = create_yule_tree_prior(
      id = "test_output_0",
      birth_rate_distr = create_uniform_distr(id = 1, upper = Inf)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})
