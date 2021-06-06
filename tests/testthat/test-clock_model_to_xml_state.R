test_that("strict", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_strict_clock_model()
    )
  )
  expected <- NULL # Indeed, nothing!
  created <- clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.4, RLN", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "state"
    )[8:9]
  )
  created <- clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.6, RLN", {
  skip("WIP 2.6 RLN params HIERO")
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expected <- unindent(
    remove_empty_lines(
      extract_xml_section_from_lines(
        readr::read_lines(get_beautier_path("rln_2_6.xml")),
        section = "state"
      )
    )[8:9]
  )
  created <- clock_model_to_xml_state(
    inference_model = inference_model
  )
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + MRCA", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(),
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      )
    )
  )

  # From rln_mrca_2_5.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco_sub\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint indeed a long line of XML
    "<stateNode id=\"rateCategories.c:anthus_aco_sub\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>" # nolint indeed a long line of XML
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("rln + MRCA with distr", {
  skip("Need create_beauti_options_v2_5")
  # From rln_mrca_one_div_x_2_5.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco_sub\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint indeed a long line of XML
    "<stateNode id=\"rateCategories.c:anthus_aco_sub\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>", # nolint indeed a long line of XML
    "<parameter id=\"ucldMean.c:anthus_aco_sub\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line of XML
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(dimension = 8),
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_one_div_x_distr()
      ),
      beauti_options = create_beauti_options_v2_5()
    )
  )

  created <- clock_model_to_xml_state(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
