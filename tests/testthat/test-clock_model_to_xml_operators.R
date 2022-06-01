test_that("strict", {
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model()
  )
  expect_true(is.null(created))
})

test_that("v2.4, RLN", {

  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("rln_2_4.xml")),
      "<operator id=.*\\.c:"
    )
  )
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.6, RLN", {

  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("rln_2_6.xml")),
      "<operator id=.*\\.c:"
    )
  )
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.6, tipdates", {
  skip("Unsure what happens here")
  check_empty_beautier_folder()

  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("tipdates_2_6.xml")),
      "<operator id=.*\\.c:test_output_0"
    )
  )
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "test_output_0")
    )
  )
  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected
  )
  expect_true(are_equivalent_xml_lines(created, expected))

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("rln + mrca", {
  skip("Need create_beauti_options_v2_5")
  # From rln_mrca_2_5.xml
  expected <- c(
    "<operator id=\"ucldStdevScaler.c:anthus_aco_sub\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco_sub\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco_sub\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco_sub\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco_sub\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>" # nolint XML
  )
  fasta_filename <- get_beautier_paths("anthus_aco_sub.fas")
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "anthus_aco_sub"),
      mrca_prior = create_mrca_prior()
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca with distr", {
  skip("Need create_beauti_options_v2_5")
  # From rln_mrca_one_div_x_2_5.xml
  expected <- c(
    "<operator id=\"ucldStdevScaler.c:anthus_aco_sub\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco_sub\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco_sub\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco_sub\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco_sub\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"ucldMeanScaler.c:anthus_aco_sub\" spec=\"ScaleOperator\" parameter=\"@ucldMean.c:anthus_aco_sub\" scaleFactor=\"0.5\" weight=\"1.0\"/>", # nolint XML
    "<operator id=\"relaxedUpDownOperator.c:anthus_aco_sub\" spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">", # nolint XML
    "    <up idref=\"ucldMean.c:anthus_aco_sub\"/>", # nolint XML
    "    <down idref=\"Tree.t:anthus_aco_sub\"/>", # nolint XML
    "</operator>" # nolint XML
  )
  fasta_filename <- get_beautier_paths("anthus_aco_sub.fas")
  created <- clock_model_to_xml_operators(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "anthus_aco_sub"),
      mrca_prior = create_mrca_prior(
        mrca_distr = create_one_div_x_distr()
      ),
      beauti_options = create_beauti_options_v2_5()
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
