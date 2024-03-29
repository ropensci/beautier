test_that("minimal, v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_strict_clock_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expected <- NULL # Indeed, nothing!
  created <- strict_clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("minimal, v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_strict_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expected <- NULL # Indeed, nothing!
  created <- strict_clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("estimated clock rate, uniform distribution, v2.6", {
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  clock_rate <- beautier::create_clock_rate_param(
    value = "0.0035", estimate = TRUE
  )
  clock_uniform <- beautier::create_uniform_distr(
    value = 0.0035, lower = 0.00277, upper = 0.00542
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(
      id = "anthus_aco_sub",
      clock_rate_param = clock_rate,
      clock_rate_distr = clock_uniform
    ),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(
      nucleotides_uppercase = TRUE
    )
  )
  # Make the inference model match the BEAUti file
  inference_model$clock_model$clock_rate_distr$id <- "0"

  created <- strict_clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_equal(1, length(created))
})


test_that("estimated clock rate, lognormal distribution, v2.6", {
  clock_rate <- beautier::create_clock_rate_param(
    value = "0.003536", estimate = TRUE
  )
  clock_rate_distr <- beautier::create_log_normal_distr(
    m = beautier::create_m_param(value = "-5.73"),
    value = "5.0"
  )

  inference_model <- create_inference_model(
    clock_model = beautier::create_strict_clock_model(
      id = NA,
      clock_rate_param = clock_rate,
      clock_rate_distr = clock_rate_distr
    )
  )

  inference_model <- init_inference_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    inference_model = inference_model
  )

  created <- strict_clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_equal(
    1,
    length(
      stringr::str_subset(
        created,
        "<parameter id=.clockRate.c:anthus_aco_sub. spec=.parameter.RealParameter. name=.stateNode.>0.003536</parameter>" # nolint indeed long
      )
    )
  )
})
