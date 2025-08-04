################################################################################
# Clock model: RLN + tipdates
################################################################################

test_that("#99: RLN + tipdates, v2.6", {
  inference_model <- create_inference_model(
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1,
      dimension = 8,
      mean_rate_prior_distr = create_uniform_distr(id = "3")
    ),
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
    beauti_options = create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint
    )
  )
  created <- create_beast2_input_from_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expected <- readLines(get_beautier_path("rln_tipdates_2_6.xml"))
  expected_line <- r"(<operator id="YuleBirthRateScaler.t:test_output_0" spec="ScaleOperator" parameter="@birthRate.t:test_output_0" scaleFactor="0.75" weight="3.0"/>)" # nolint
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))
  expected_line <- r"(<prior id="MeanRatePrior.c:test_output_0" name="distribution" x="@ucldMean.c:test_output_0">)" # nolint
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))
  expected_line <- r"(<Uniform id="Uniform.3" name="distr" upper="Infinity"/>)"
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))
  expected_line <- r"(<operator id="ucldMeanScaler.c:test_output_0" spec="ScaleOperator" parameter="@ucldMean.c:test_output_0" scaleFactor="0.5" weight="1.0"/>)" # nolint
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))
  expected_line <- r"(<operator id="relaxedUpDownOperator.c:test_output_0" spec="UpDownOperator" scaleFactor="0.75" weight="3.0">)" # nolint
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))
  expected_line <- r"(<trait id="dateTrait.t:test_output_0" spec="beast.evolution.tree.TraitSet" traitname="date" value="">)" # nolint
  testthat::expect_equal(1, sum(stringr::str_detect(expected, expected_line)))
  testthat::expect_equal(1, sum(stringr::str_detect(created, expected_line)))


  if (1 == 2) {
    compare_lines(
      lines = created,
      expected = expected,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml"
    )
  }
  expect_true(are_equivalent_xml_lines(created, expected))

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("rln_uclstdev_beta_2_4.xml", {

  created <- create_beast2_input_from_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_beta_distr(
          id = 0,
          alpha = create_alpha_param(id = 4, value = "2.0"),
          beta = create_beta_param(id = 5, value = "2.0")
        ),
        mparam_id = 1
      ),
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )
  expected <- readLines(get_beautier_path("rln_uclstdev_beta_2_4.xml"))

  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(are_equivalent_xml_lines(created, expected))

  remove_beautier_folder()
  check_empty_beautier_folder()
})

################################################################################
# Clock model: strict
################################################################################

test_that("strict_clock_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("strict_clock_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("strict_clock_rate_0_5_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    clock_model = create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(
        id = "test_output_0.fas",
        value = "0.5"
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("strict_clock_rate_0_5_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})
