context("clock_models_to_xml_prior_distr")

test_that("strict", {

  expect_true(
    is.null(
      clock_models_to_xml_prior_distr(
        list(create_strict_clock_model()) # Don't even need an ID
      )
    )
  )
})

test_that("rln", {

  expected <- c(
    "<prior id=\"ucldStdevPrior.c:test_output_0\" name=\"distribution\" x=\"@ucldStdev.c:test_output_0\">", # nolint XML
    "    <Gamma id=\"Gamma.0\" name=\"distr\">",
    "        <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML
    "        <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML
    "    </Gamma>",
    "</prior>"

  )
  created <- clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "test_output_0",
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
