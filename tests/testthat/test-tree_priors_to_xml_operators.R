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
  created <- tree_priors_to_xml_operators(
    tree_priors = list(
      create_yule_tree_prior(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
