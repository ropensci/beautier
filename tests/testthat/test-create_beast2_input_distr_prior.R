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
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 100),
        mparam_id = 1
      )
    )
  )
  created <- create_beast2_input_distr_prior(
    site_models = list(inference_model$site_model),
    clock_models = list(inference_model$clock_model),
    tree_priors = list(inference_model$tree_prior)
  )
  expect_equal(created, expected)
})

test_that("Yule", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">",
    "    <distribution id=\"YuleModel.t:test_output_0\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:test_output_0\" tree=\"@Tree.t:test_output_0\"/>", # nolint long line indeed
    "    <prior id=\"YuleBirthRatePrior.t:test_output_0\" name=\"distribution\" x=\"@birthRate.t:test_output_0\">", # nolint long line indeed
    "        <Uniform id=\"Uniform.100\" name=\"distr\" upper=\"Infinity\"/>",
    "    </prior>",
    "</distribution>"
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )

  created <- create_beast2_input_distr_prior(
    site_models = list(inference_model$site_model),
    clock_models = list(inference_model$clock_model),
    tree_priors = list(inference_model$tree_prior)
  )
  expect_equal(created, expected)
})
