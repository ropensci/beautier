test_that("RLN, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "branchRateModel"
    )
  )
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 2, value = "0.5396"),
          beta = create_beta_param(id = 3, value = "0.3819")
        ),
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        mparam_id = 1
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_branch_rate_model_rln_xml(
    inference_model = inference_model
  )
  expect_equal(created, expected)
})

test_that("RLN, v2.6", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
      section = "branchRateModel"
    )
  )
  inference_model <- create_test_inference_model(
    clock_model = create_rln_clock_model(
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
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = inference_model
  )
  created <- create_branch_rate_model_rln_xml(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("RLN + tipdates, v2.6", {
  skip("https://github.com/ropensci/babette/issues/99, RLN + tipdates, v2.6")
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_tipdates_2_6.xml")),
      section = "branchRateModel"
    )
  )
  inference_model <- create_inference_model(
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
    beauti_options = create_beauti_options_v2_6()
  )
  created <- create_branch_rate_model_rln_xml(
    inference_model = inference_model
  )
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml",
  )
  expect_equal(created[1], expected[1])
  expect_true(are_equivalent_xml_lines(created, expected))
})
