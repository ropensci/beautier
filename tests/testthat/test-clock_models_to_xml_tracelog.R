context("clock_models_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.character(
      beautier:::clock_models_to_xml_tracelog(
        create_strict_clock_models(ids = "anthus_aco")
      )
    )
  )

})
