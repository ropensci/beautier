context("site_models_to_xml_tracelog")

test_that("creates a text or NULL", {

  testthat::expect_true(
    is.null(
      beautier:::site_models_to_xml_tracelog(
        list(create_jc69_site_model(id = "anthus_aco"))
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_models_to_xml_tracelog(
        list(create_hky_site_model(id = "anthus_aco"))
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_models_to_xml_tracelog(
        list(create_tn93_site_model(id = "anthus_aco"))
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_models_to_xml_tracelog(
        list(create_gtr_site_model(id = "anthus_aco"))
      )
    )
  )

})
