context("clock_models_to_xml_tracelog")

test_that("creates a text or NULL", {

  testthat::expect_true(
    is.null(
      beautier:::clock_models_to_xml_tracelog(
        create_strict_clock_models(ids = "anthus_aco")
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_models_to_xml_tracelog(
        list(create_rln_clock_model(id = "anthus_aco"))
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_models_to_xml_tracelog(
        create_strict_clock_models(ids = c("anthus_aco", "anthus_nd2"))
      )
    )
  )

})

################################################################################
# Single alignment
################################################################################

test_that("RLN", {

  expected <- c(
    "<log idref=\"ucldStdev.c:test_output_0\"/>", # nolint XML
    "<log id=\"rate.c:test_output_0\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:test_output_0\" tree=\"@Tree.t:test_output_0\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
