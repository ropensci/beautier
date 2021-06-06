################################################################################
# Clock model: RLN
################################################################################

test_that("rln_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("rln_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln_2_6.xml", {
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options_v2_6()
  )
  created <- create_beast2_input_from_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expected <- readLines(get_beautier_path("rln_2_6.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln_uclstdev_beta_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 4, value = "2.0"),
        beta = create_beta_param(id = 5, value = "2.0")
      ),
      mparam_id = 1
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(get_beautier_path("rln_uclstdev_beta_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Clock model: strict
################################################################################

test_that("strict_clock_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(get_beautier_path("strict_clock_rate_0_5_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})
