context("clock_models_to_xml_tracelog")

test_that("creates a text", {

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
