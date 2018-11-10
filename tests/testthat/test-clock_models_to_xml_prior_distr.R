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

test_that("rln + mrca with distr", {

  # From 'rln_mrca_one_div_x_2_5.xml'
  expected <- c(
    "<prior id=\"MeanRatePrior.c:anthus_aco_sub\" name=\"distribution\" x=\"@ucldMean.c:anthus_aco_sub\">", # nolint XML can be long
    "    <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>",
    "</prior>",
    "<prior id=\"ucldStdevPrior.c:anthus_aco_sub\" name=\"distribution\" x=\"@ucldStdev.c:anthus_aco_sub\">", # nolint XML can be long
    "    <Gamma id=\"Gamma.0\" name=\"distr\">",
    "        <parameter id=\"RealParameter.2\" estimate=\"false\" name=\"alpha\">0.5396</parameter>", # nolint XML can be long
    "        <parameter id=\"RealParameter.3\" estimate=\"false\" name=\"beta\">0.3819</parameter>", # nolint XML can be long
    "    </Gamma>",
    "</prior>"

  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  created <- clock_models_to_xml_prior_distr(
    list(
      create_rln_clock_model(
        id = "anthus_aco_sub",
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 3),
        mparam_id = 1
      )
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
