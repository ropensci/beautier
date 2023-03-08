test_that("Yule, v2.4", {
  expected <- stringr::str_trim(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("2_4.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model()
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Yule, v2.6", {
  expected <- stringr::str_trim(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("anthus_aco_sub_2_6.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model(
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    inference_model = inference_model
  )
  created <- tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("RLN + tipdates, v2.6", {
  # Irreproducible error
  skip("'scaleFactor' is in BEAUti file, using an unknown version of BEAUti")
  inference_model <- create_inference_model(
    tree_prior = create_yule_tree_prior(
      id = "test_output_0",
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
    beauti_options = create_beauti_options_v2_6()
  )
  created <- tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expected <- stringr::str_subset(
    stringr::str_trim(
      stringr::str_subset(
        readr::read_lines(get_beautier_path("rln_tipdates_2_6.xml")),
        "<operator id="
      )
    ),
    "\\.t:test_output_0"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
