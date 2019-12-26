context("tree_priors_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.character(
      tree_priors_to_xml_tracelog(
        list(create_yule_tree_prior(id = "anthus_aco"))
      )
    )
  )
})

test_that("BD", {

  expected <- c(
    "<log idref=\"BirthDeath.t:test_output_0\"/>", # nolint XML
    "<log idref=\"BDBirthRate.t:test_output_0\"/>", # nolint XML
    "<log idref=\"BDDeathRate.t:test_output_0\"/>" # nolint XML
  )
  created <- tree_priors_to_xml_tracelog(
    list(
      create_bd_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("CBS", {

  expected <- c(
    "<log idref=\"BayesianSkyline.t:test_output_0\"/>", # nolint XML
    "<log idref=\"bPopSizes.t:test_output_0\"/>", # nolint XML
    "<log idref=\"bGroupSizes.t:test_output_0\"/>" # nolint XML
  )
  created <- tree_priors_to_xml_tracelog(
    list(
      create_cbs_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("CCP", {

  expected <- c(
    "<log idref=\"popSize.t:test_output_0\"/>", # nolint XML
    "<log idref=\"CoalescentConstant.t:test_output_0\"/>" # nolint XML
  )
  created <- tree_priors_to_xml_tracelog(
    list(
      create_ccp_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("CEP", {

  expected <- c(
    "<log idref=\"CoalescentExponential.t:test_output_0\"/>", # nolint XML
    "<log idref=\"ePopSize.t:test_output_0\"/>", # nolint XML
    "<log idref=\"growthRate.t:test_output_0\"/>" # nolint XML
  )
  created <- tree_priors_to_xml_tracelog(
    list(
      create_cep_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Yule", {

  expected <- c(
    "<log idref=\"YuleModel.t:test_output_0\"/>", # nolint XML
    "<log idref=\"birthRate.t:test_output_0\"/>" # nolint XML
  )
  created <- tree_priors_to_xml_tracelog(
    list(
      create_yule_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})
