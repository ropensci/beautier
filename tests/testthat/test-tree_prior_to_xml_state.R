test_that("use, yule", {
  tree_prior <- init_tree_priors(
    ids = "test_output_0",
    list(create_yule_tree_prior())
  )[[1]]
  created <- tree_prior_to_xml_state(tree_prior = tree_prior)
  expected <- "<parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expect_equal(created, expected)
})

test_that("use, BD", {
  tree_prior <- init_tree_priors(
    ids = "test_output_0",
    list(create_bd_tree_prior())
  )[[1]]
  created <- tree_prior_to_xml_state(tree_prior = tree_prior)
  expected <- c(
    "<parameter id=\"BDBirthRate.t:test_output_0\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint indeed a long line
    "<parameter id=\"BDDeathRate.t:test_output_0\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, CBS", {
  tree_prior <- init_tree_priors(
    ids = "test_output_0",
    list(create_cbs_tree_prior())
  )[[1]]
  created <- tree_prior_to_xml_state(tree_prior = tree_prior)
  expected <- c(
    "<parameter id=\"bPopSizes.t:test_output_0\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint indeed a long line
    "<stateNode id=\"bGroupSizes.t:test_output_0\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})


test_that("use, CCP", {
  tree_prior <- init_tree_priors(
    ids = "test_output_0",
    list(create_ccp_tree_prior())
  )[[1]]
  created <- tree_prior_to_xml_state(tree_prior = tree_prior)
  expected <- c(
    "<parameter id=\"popSize.t:test_output_0\" name=\"stateNode\">0.3</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})

test_that("use, CEP", {
  tree_prior <- init_tree_priors(
    ids = "test_output_0",
    list(create_cep_tree_prior())
  )[[1]]
  created <- tree_prior_to_xml_state(tree_prior = tree_prior)
  expected <- c(
    "<parameter id=\"ePopSize.t:test_output_0\" name=\"stateNode\">0.3</parameter>", # nolint indeed a long line
    "<parameter id=\"growthRate.t:test_output_0\" name=\"stateNode\">3.0E-4</parameter>" # nolint indeed a long line
  )
  expect_equal(created, expected)
})
