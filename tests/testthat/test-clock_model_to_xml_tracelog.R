test_that("creates a text", {
  expect_true(
    is.null(
      clock_model_to_xml_tracelog(
        create_inference_model(
          clock_model = create_strict_clock_model(id = "anthus_aco")
        )
      )
    )
  )
})

test_that("rln", {
  expected <- c(
    "<log idref=\"ucldStdev.c:test_output_0\"/>", # nolint XML
    "<log id=\"rate.c:test_output_0\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:test_output_0\" tree=\"@Tree.t:test_output_0\"/>" # nolint XML
  )
  created <- clock_model_to_xml_tracelog(
    inference_model = create_inference_model(
      clock_model = create_rln_clock_model(id = "test_output_0")
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
