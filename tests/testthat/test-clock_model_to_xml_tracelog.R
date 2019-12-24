context("clock_model_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.null(
      clock_model_to_xml_tracelog(
        create_strict_clock_model(id = "anthus_aco")
      )
    )
  )

})
