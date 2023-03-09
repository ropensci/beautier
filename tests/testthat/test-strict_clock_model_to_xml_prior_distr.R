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

test_that("strict, v2.6, with clock rate to be estimated", {

  clock_model <- beautier::create_strict_clock_model(
    id = "anthus_aco_sub",
    clock_rate_param = beautier::create_clock_rate_param(value = "0.0035", estimate = TRUE),
    clock_rate_distr = beautier::create_uniform_distr(id = 0, value = 0.0035, lower = 0.00277, upper = 0.00542)
  )

  # <prior id="ClockPrior.c:anthus_aco_sub" name="distribution" x="@clockRate.c:anthus_aco_sub">
  #   <Uniform id="Uniform.0" name="distr" upper="Infinity"/>
  # </prior>
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
