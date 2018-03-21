context("clock_model_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.null(
      beautier:::clock_model_to_xml_tracelog(
        create_strict_clock_model(id = "anthus_aco"),
        is_first = TRUE
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_model_to_xml_tracelog(
        create_rln_clock_model(id = "anthus_aco"),
        is_first = TRUE
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_model_to_xml_tracelog(
        create_rln_clock_model(id = "anthus_aco"),
        is_first = FALSE
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::clock_model_to_xml_tracelog(
        create_strict_clock_model(id = "anthus_aco"),
        is_first = FALSE
      )
    )
  )

})
