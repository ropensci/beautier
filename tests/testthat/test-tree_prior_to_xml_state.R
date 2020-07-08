test_that("use, BD", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_bd_tree_prior()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- c(
    "<parameter id=\"BDBirthRate.t:test_output_0\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint indeed a long line
    "<parameter id=\"BDDeathRate.t:test_output_0\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, CBS", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_cbs_tree_prior()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- c(
    "<parameter id=\"bPopSizes.t:test_output_0\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint indeed a long line
    "<stateNode id=\"bGroupSizes.t:test_output_0\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, CCP", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_ccp_tree_prior()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- c(
    "<parameter id=\"popSize.t:test_output_0\" name=\"stateNode\">0.3</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, CEP", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_cep_tree_prior()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- c(
    "<parameter id=\"ePopSize.t:test_output_0\" name=\"stateNode\">0.3</parameter>", # nolint indeed a long line
    "<parameter id=\"growthRate.t:test_output_0\" name=\"stateNode\">3.0E-4</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, yule, v2.4", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- "<parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expect_equal(created, expected)
})

test_that("use, yule, v2.6", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- tree_prior_to_xml_state(inference_model = inference_model)
  expected <- "<parameter id=\"birthRate.t:test_output_0\" spec=\"parameter.RealParameter\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expect_equal(created, expected)
})

test_that("Deprecation", {
  expect_error(
    tree_prior_to_xml_state(
      tree_prior = "something",
      inference_model = "irrelevant"
    ),
    "'tree_prior' is deprecated, use 'inference_model' instead"
  )
})
