test_that("rln, v2.4", {

  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "distribution"
    )[7:12]
  )
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(
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

  created <- rln_clock_model_to_xml_prior_distr(inference_model)
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln, v2.6", {
  expected <- unindent(
    remove_empty_lines(
      extract_xml_section_from_lines(
        lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
        section = "distribution"
      )[13:23]
    )
  )
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 1),
      mparam_id = 1
    ),
    beauti_options = create_beauti_options_v2_6()
  )

  created <- rln_clock_model_to_xml_prior_distr(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + mrca with distr", {
  # <prior id=\"MeanRatePrior.c:[...]> # nolint
  # </prior> # nolint
  # <prior id=\"ucldStdevPrior.c:[...]> # nolint
  # </prior> # nolint
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(
        get_beautier_path("rln_mrca_one_div_x_2_5.xml")
      ),
      section = "distribution"
    )[7:15]
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(
      id = "anthus_aco_sub",
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mean_rate_prior_distr = create_uniform_distr(id = 3),
      mparam_id = 1
    ),
    mrca_prior = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_one_div_x_distr()
    )
  )
  created <- rln_clock_model_to_xml_prior_distr(inference_model)
  expect_true(are_equivalent_xml_lines(created, expected))
})
