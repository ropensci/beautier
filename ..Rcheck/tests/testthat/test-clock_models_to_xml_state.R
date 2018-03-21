context("clock_models_to_xml_state")

test_that("strict strict strict strict", {

  expected <- c(
    # Note the absence of anthus_aco
    "<parameter id=\"clockRate.c:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd4\" name=\"stateNode\">1.0</parameter>"  # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2"),
      create_strict_clock_model(id = "anthus_nd3"),
      create_strict_clock_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("RLN RLN RLN RLN", {

  expected <- c(
    # note the absence of ucldMean.c for anthus_aco
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd2\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd3\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd4\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd4\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_rln_clock_model(id = "anthus_nd2", dimension = 42),
      create_rln_clock_model(id = "anthus_nd3", dimension = 42),
      create_rln_clock_model(id = "anthus_nd4", dimension = 42)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})


test_that("strict, RLN, strict, RLN", {

  expected <- c(
    # Note the absence of anthus_aco
    "<parameter id=\"ucldMean.c:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd2\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd4\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd4\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2", dimension = 42),
      create_strict_clock_model(id = "anthus_nd3"),
      create_rln_clock_model(id = "anthus_nd4", dimension = 42)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("RLN, strict, RLN, strict", {

  expected <- c(
    # note the absence of ucldMean.c for anthus_aco
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd3\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd4\" name=\"stateNode\">1.0</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_strict_clock_model(id = "anthus_nd2"),
      create_rln_clock_model(id = "anthus_nd3", dimension = 42),
      create_strict_clock_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})



test_that("shared strict clock by (aco, nd2, nd3, nd4)", {

  expected <- c(
    # None at all
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("shared RLN clock", {

  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>" # nolint XML is long, so this line is long

  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_rln_clock_model(id = "anthus_aco", dimension = 42)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("shared strict clocks by (aco, nd2) and (nd3, nd4)", {

  expected <- c(
    "<parameter id=\"clockRate.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd3"),
      create_strict_clock_model(id = "anthus_nd3")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("(aco, nd2) share RLN clocks, (nd3, nd4) share strict clocks", {

  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_rln_clock_model(id = "anthus_aco", dimension = 42),
      create_strict_clock_model(id = "anthus_nd3"),
      create_strict_clock_model(id = "anthus_nd3")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("strict 1.1 strict 1.2 strict 1.3 strict 1.4", {

  expected <- c(
    # Note the absence of anthus_aco
    "<parameter id=\"clockRate.c:anthus_nd2\" name=\"stateNode\">1.2</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd3\" name=\"stateNode\">1.3</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"clockRate.c:anthus_nd4\" name=\"stateNode\">1.4</parameter>"  # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco",
        clock_rate_param = create_clock_rate_param(value = "1.1")),
      create_strict_clock_model(id = "anthus_nd2",
        clock_rate_param = create_clock_rate_param(value = "1.2")),
      create_strict_clock_model(id = "anthus_nd3",
        clock_rate_param = create_clock_rate_param(value = "1.3")),
      create_strict_clock_model(id = "anthus_nd4",
        clock_rate_param = create_clock_rate_param(value = "1.4"))
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("RLN -1 RLN 0 RLN 1 RLN 2 rates", {

  # Does not matter for the state section
  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd2\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd3\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd4\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd4\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>" # nolint XML is long, so this line is long
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(id = "anthus_aco",
        n_rate_categories = -1, dimension = 42),
      create_rln_clock_model(id = "anthus_nd2",
        n_rate_categories = 0, dimension = 42),
      create_rln_clock_model(id = "anthus_nd3",
        n_rate_categories = 1, dimension = 42),
      create_rln_clock_model(id = "anthus_nd4",
        n_rate_categories = 2, dimension = 42)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("RLN 1.1 RLN 1.2 RLN 1.3 RLN 1.4 clock rates", {

  expected <- c(
    "<parameter id=\"ucldStdev.c:anthus_aco\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_aco\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd2\" name=\"stateNode\">1.2</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd2\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd3\" name=\"stateNode\">1.3</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd3\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldMean.c:anthus_nd4\" name=\"stateNode\">1.4</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"ucldStdev.c:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>", # nolint XML is long, so this line is long
    "<stateNode id=\"rateCategories.c:anthus_nd4\" spec=\"parameter.IntegerParameter\" dimension=\"42\">1</stateNode>" # nolint XML is long, so this line is long

  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(
      create_rln_clock_model(
        id = "anthus_aco", mean_clock_rate = "1.1", dimension = 42),
      create_rln_clock_model(
        id = "anthus_nd2", mean_clock_rate = "1.2", dimension = 42),
      create_rln_clock_model(
        id = "anthus_nd3", mean_clock_rate = "1.3", dimension = 42),
      create_rln_clock_model(
        id = "anthus_nd4", mean_clock_rate = "1.4", dimension = 42)
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
