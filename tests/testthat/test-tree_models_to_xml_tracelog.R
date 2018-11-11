context("tree_models_to_xml_tracelog")

test_that("creates a text", {

  expect_true(
    is.character(
      tree_models_to_xml_tracelog(
        list(create_jc69_site_model(id = "anthus_aco"))
      )
    )
  )

})
