context("clock_models_to_xml_operators")

test_that("strict", {

  created <- clock_models_to_xml_operators(
    clock_models = list(
      create_strict_clock_model()
    )
  )
  expect_true(is.null(created))
})

test_that("rln", {

  # From rln_2_4.xml
  expected <- c(
    "<operator id=\"ucldStdevScaler.c:test_output_0\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:test_output_0\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:test_output_0\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:test_output_0\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:test_output_0\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>" # nolint XML
  )
  created <- clock_models_to_xml_operators(
    clock_models = list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca", {

  # From rln_mrca_2_5.xml
  expected <- c(
    "<operator id=\"ucldStdevScaler.c:anthus_aco_sub\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco_sub\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco_sub\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco_sub\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco_sub\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco_sub\" weight=\"10.0\"/>" # nolint XML
  )
  fasta_filename <- get_beautier_paths("anthus_aco_sub.fas")
  created <- clock_models_to_xml_operators(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco_sub")
    ),
    mrca_priors = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename)
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca with distr", {

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
  created <- clock_models_to_xml_operators(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco_sub")
    ),
    mrca_priors = list(
        create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
