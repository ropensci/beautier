context("tree_models_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.character(
      beautier:::tree_models_to_xml_tracelog(
        create_jc69_site_models(ids = "anthus_aco")
      )
    )
  )

})
