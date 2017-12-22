context("tree_priors_to_xml_operators")

test_that("Yule", {

  expected <- c(
    "<operator id=\"YuleBirthRateScaler.t:test_output_0\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:test_output_0\" scaleFactor=\"0.75\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeScaler.t:test_output_0\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:test_output_0\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeRootScaler.t:test_output_0\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" tree=\"@Tree.t:test_output_0\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelUniformOperator.t:test_output_0\" spec=\"Uniform\" tree=\"@Tree.t:test_output_0\" weight=\"30.0\"/>", # nolint XML
    "<operator id=\"YuleModelSubtreeSlide.t:test_output_0\" spec=\"SubtreeSlide\" tree=\"@Tree.t:test_output_0\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelNarrow.t:test_output_0\" spec=\"Exchange\" tree=\"@Tree.t:test_output_0\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelWide.t:test_output_0\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:test_output_0\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelWilsonBalding.t:test_output_0\" spec=\"WilsonBalding\" tree=\"@Tree.t:test_output_0\" weight=\"3.0\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_operators(
    tree_priors = list(
      create_yule_tree_prior(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("Yule Yule", {

  expected <- c(
    "<operator id=\"YuleBirthRateScaler.t:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:anthus_aco\" scaleFactor=\"0.75\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeScaler.t:anthus_aco\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:anthus_aco\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeRootScaler.t:anthus_aco\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" tree=\"@Tree.t:anthus_aco\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelUniformOperator.t:anthus_aco\" spec=\"Uniform\" tree=\"@Tree.t:anthus_aco\" weight=\"30.0\"/>", # nolint XML
    "<operator id=\"YuleModelSubtreeSlide.t:anthus_aco\" spec=\"SubtreeSlide\" tree=\"@Tree.t:anthus_aco\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelNarrow.t:anthus_aco\" spec=\"Exchange\" tree=\"@Tree.t:anthus_aco\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelWide.t:anthus_aco\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:anthus_aco\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelWilsonBalding.t:anthus_aco\" spec=\"WilsonBalding\" tree=\"@Tree.t:anthus_aco\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleBirthRateScaler.t:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:anthus_nd2\" scaleFactor=\"0.75\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeScaler.t:anthus_nd2\" spec=\"ScaleOperator\" scaleFactor=\"0.5\" tree=\"@Tree.t:anthus_nd2\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelTreeRootScaler.t:anthus_nd2\" spec=\"ScaleOperator\" rootOnly=\"true\" scaleFactor=\"0.5\" tree=\"@Tree.t:anthus_nd2\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelUniformOperator.t:anthus_nd2\" spec=\"Uniform\" tree=\"@Tree.t:anthus_nd2\" weight=\"30.0\"/>", # nolint XML
    "<operator id=\"YuleModelSubtreeSlide.t:anthus_nd2\" spec=\"SubtreeSlide\" tree=\"@Tree.t:anthus_nd2\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelNarrow.t:anthus_nd2\" spec=\"Exchange\" tree=\"@Tree.t:anthus_nd2\" weight=\"15.0\"/>", # nolint XML
    "<operator id=\"YuleModelWide.t:anthus_nd2\" spec=\"Exchange\" isNarrow=\"false\" tree=\"@Tree.t:anthus_nd2\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"YuleModelWilsonBalding.t:anthus_nd2\" spec=\"WilsonBalding\" tree=\"@Tree.t:anthus_nd2\" weight=\"3.0\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_operators(
    tree_prior = list(
      create_yule_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("interface: support multiple fixed_crown_ages", {

  testthat::expect_silent(
    beautier:::tree_priors_to_xml_operators(
      tree_prior = list(
        create_yule_tree_prior(id = "anthus_aco"),
        create_yule_tree_prior(id = "anthus_nd2")
      ),
      fixed_crown_ages = c(TRUE, TRUE)
    )
  )
})
