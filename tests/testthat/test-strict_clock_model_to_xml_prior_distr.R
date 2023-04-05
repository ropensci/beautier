test_that("minimal, v2.6", {
  expect_true(
    is.null(
      strict_clock_model_to_xml_prior_distr(
        inference_model = create_inference_model(
          clock_model = create_strict_clock_model(),
          beauti_options = create_beauti_options_v2_4()
        )
      )
    )
  )
})

test_that("minimal, v2.6", {
  expect_true(
    is.null(
      strict_clock_model_to_xml_prior_distr(
        inference_model = create_inference_model(
          clock_model = create_strict_clock_model(),
          beauti_options = create_beauti_options_v2_6()
        )
      )
    )
  )
})

test_that("strict, v2.6, with clock rate to be estimated, uniform distr", {

  clock_model <- beautier::create_strict_clock_model(
    id = "anthus_aco_sub",
    clock_rate_param = beautier::create_clock_rate_param(
      value = "0.0035", estimate = TRUE
    ),
    clock_rate_distr = beautier::create_uniform_distr(
      id = 0, value = 0.0035, lower = 0.00277, upper = 0.00542
    )
  )

  expect_equal(
    3,
    length(
      strict_clock_model_to_xml_prior_distr(
        inference_model = create_inference_model(
          clock_model = clock_model,
          beauti_options = create_beauti_options_v2_6()
        )
      )
    )
  )
})

test_that("strict, v2.6, with clock rate to be estimated, lognormal distr", {

  clock_rate <- beautier::create_clock_rate_param(
    value = "0.003536", estimate = TRUE
  )
  clock_rate_distr <- beautier::create_log_normal_distr(
    id = 1,
    m = beautier::create_m_param(id = "3", value = "-5.73"),
    s = beautier::create_s_param(
      id = "4", value = "0.5", lower = "0.0", upper = "5.0"
    ),
    value = "5.0"
  )
  clock_model <- beautier::create_strict_clock_model(
    id = "anthus_aco_sub",
    clock_rate_param = clock_rate,
    clock_rate_distr = clock_rate_distr
  )
  created <- strict_clock_model_to_xml_prior_distr(
    inference_model = create_inference_model(
      clock_model = clock_model,
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expect_equal(
    6,
    length(created)
  )
})
