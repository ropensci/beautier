context("clock_models_to_xml_state")

test_that("strict", {

  # From anthus_aco_sub.xml
  expected <- NULL # Indeed, nothing!
  created <- clock_models_to_xml_state(
    clock_models = list(create_strict_clock_model(id = "anthus_aco_sub"))
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln", {

  # From rln_2_4.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:test_output_0\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint indeed a long line of XML
    "<stateNode id=\"rateCategories.c:test_output_0\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>" # nolint indeed a long line of XML
  )
  created <- clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "test_output_0", dimension = 8)
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("rln + MRCA", {

  # From rln_mrca_2_5.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco_sub\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint indeed a long line of XML
    "<stateNode id=\"rateCategories.c:anthus_aco_sub\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>" # nolint indeed a long line of XML
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco_sub", dimension = 8)
    ),
    mrca_priors = list(
      create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      )
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("rln + MRCA with distr", {

  # From rln_mrca_one_div_x_2_5.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco_sub\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint indeed a long line of XML
    "<stateNode id=\"rateCategories.c:anthus_aco_sub\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>", # nolint indeed a long line of XML
    "<parameter id=\"ucldMean.c:anthus_aco_sub\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line of XML
  )
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco_sub", dimension = 8)
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
