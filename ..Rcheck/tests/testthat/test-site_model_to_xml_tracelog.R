context("site_model_to_xml_tracelog")

test_that("creates a text or nothing", {

  testthat::expect_true(
    is.null(
      beautier:::site_model_to_xml_tracelog(
        create_jc69_site_model(id = "anthus_aco")
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_model_to_xml_tracelog(
        create_hky_site_model(id = "anthus_aco")
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_model_to_xml_tracelog(
        create_tn93_site_model(id = "anthus_aco")
      )
    )
  )

  testthat::expect_true(
    is.character(
      beautier:::site_model_to_xml_tracelog(
        create_gtr_site_model(id = "anthus_aco")
      )
    )
  )

})
