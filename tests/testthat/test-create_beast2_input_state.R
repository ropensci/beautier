test_that("v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("2_4.xml")),
      section = "state"
    )
  )
  expect_equal(created, expected)
})

test_that("v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("2_6_2.xml")),
      section = "state"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.6, RLN", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
      section = "state"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tipdates, v2.6", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("tipdates_2_6.xml")),
      section = "state"
    )
  )
  if (1 == 2) {
    compare_lines(
      lines = created,
      expected = expected,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml",
    )
  }

  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("RLN + tipdates, v2.6", {
  inference_model <- create_inference_model(
    site_model = create_jc69_site_model(id = "test_output_0"),
    tree_prior = create_yule_tree_prior(
      id = "test_output_0",
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      id = "test_output_0",
      ucldstdev_distr = create_gamma_distr(
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1,
      dimension = 8
    ),
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
    beauti_options = create_beauti_options_v2_6()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_tipdates_2_6.xml")),
      section = "state"
    )
  )
  if (1 == 2) {
    compare_lines(
      lines = created,
      expected = expected,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml",
    )
  }
  expect_true(are_equivalent_xml_lines(created, expected))
})
