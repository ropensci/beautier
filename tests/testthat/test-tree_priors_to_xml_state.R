context("tree_priors_to_xml_state")

test_that("Yule Yule Yule Yule", {

  expected <- c(
    "<parameter id=\"birthRate.t:anthus_aco\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd4\" name=\"stateNode\">1.0</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_yule_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2"),
      create_yule_tree_prior(id = "anthus_nd3"),
      create_yule_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("BD BD BD BD", {

  expected <- c(
    "<parameter id=\"BDBirthRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd2\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd2\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd3\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd3\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_bd_tree_prior(id = "anthus_aco"),
      create_bd_tree_prior(id = "anthus_nd2"),
      create_bd_tree_prior(id = "anthus_nd3"),
      create_bd_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("CBS CBS CBS CBS", {

  expected <- c(
      "<parameter id=\"bPopSizes.t:anthus_aco\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint XML can be long
      "<stateNode id=\"bGroupSizes.t:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>", # nolint XML can be long
      "<parameter id=\"bPopSizes.t:anthus_nd4\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint XML can be long
      "<stateNode id=\"bGroupSizes.t:anthus_nd4\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>", # nolint XML can be long
      "<parameter id=\"bPopSizes.t:anthus_nd2\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint XML can be long
      "<stateNode id=\"bGroupSizes.t:anthus_nd2\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>", # nolint XML can be long
      "<parameter id=\"bPopSizes.t:anthus_nd3\" dimension=\"5\" lower=\"0.0\" name=\"stateNode\" upper=\"380000.0\">380.0</parameter>", # nolint XML can be long
      "<stateNode id=\"bGroupSizes.t:anthus_nd3\" spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_cbs_tree_prior(id = "anthus_aco"),
      create_cbs_tree_prior(id = "anthus_nd2"),
      create_cbs_tree_prior(id = "anthus_nd3"),
      create_cbs_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CCP CCP CCP CCP", {

  expected <- c(
    "<parameter id=\"popSize.t:anthus_nd2\" name=\"stateNode\">0.3</parameter>",
    "<parameter id=\"popSize.t:anthus_nd3\" name=\"stateNode\">0.3</parameter>",
    "<parameter id=\"popSize.t:anthus_nd4\" name=\"stateNode\">0.3</parameter>",
    "<parameter id=\"popSize.t:anthus_aco\" name=\"stateNode\">0.3</parameter>"
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_ccp_tree_prior(id = "anthus_aco"),
      create_ccp_tree_prior(id = "anthus_nd2"),
      create_ccp_tree_prior(id = "anthus_nd3"),
      create_ccp_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CEP CEP CEP CEP", {

  expected <- c(
    "<parameter id=\"ePopSize.t:anthus_aco\" name=\"stateNode\">0.3</parameter>", # nolint XML can be long
    "<parameter id=\"growthRate.t:anthus_aco\" name=\"stateNode\">3.0E-4</parameter>", # nolint XML can be long
    "<parameter id=\"ePopSize.t:anthus_nd4\" name=\"stateNode\">0.3</parameter>", # nolint XML can be long
    "<parameter id=\"growthRate.t:anthus_nd4\" name=\"stateNode\">3.0E-4</parameter>", # nolint XML can be long
    "<parameter id=\"ePopSize.t:anthus_nd3\" name=\"stateNode\">0.3</parameter>", # nolint XML can be long
    "<parameter id=\"growthRate.t:anthus_nd3\" name=\"stateNode\">3.0E-4</parameter>", # nolint XML can be long
    "<parameter id=\"ePopSize.t:anthus_nd2\" name=\"stateNode\">0.3</parameter>", # nolint XML can be long
    "<parameter id=\"growthRate.t:anthus_nd2\" name=\"stateNode\">3.0E-4</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_cep_tree_prior(id = "anthus_aco"),
      create_cep_tree_prior(id = "anthus_nd2"),
      create_cep_tree_prior(id = "anthus_nd3"),
      create_cep_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})



test_that("Yule Yule Yule BD", {

  expected <- c(
    "<parameter id=\"birthRate.t:anthus_aco\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_yule_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2"),
      create_yule_tree_prior(id = "anthus_nd3"),
      create_bd_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("Yule Yule BD BD", {

  expected <- c(
    "<parameter id=\"birthRate.t:anthus_aco\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd3\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd3\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_yule_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2"),
      create_bd_tree_prior(id = "anthus_nd3"),
      create_bd_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("BD Yule Yule BD", {

  expected <- c(
    "<parameter id=\"BDBirthRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_bd_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2"),
      create_yule_tree_prior(id = "anthus_nd3"),
      create_bd_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})


test_that("BD BD Yule Yule", {

  expected <- c(
    "<parameter id=\"birthRate.t:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_aco\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"BDBirthRate.t:anthus_nd2\" lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"BDDeathRate.t:anthus_nd2\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>", # nolint XML can be long
    "<parameter id=\"birthRate.t:anthus_nd4\" name=\"stateNode\">1.0</parameter>" # nolint XML can be long
  )
  created <- beautier:::tree_priors_to_xml_state(
    tree_priors = list(
      create_bd_tree_prior(id = "anthus_aco"),
      create_bd_tree_prior(id = "anthus_nd2"),
      create_yule_tree_prior(id = "anthus_nd3"),
      create_yule_tree_prior(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})
