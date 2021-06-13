test_that("minimal use", {
  expect_silent(
    tree_model_to_tracelog_xml(
      inference_model = create_inference_model(
        site_model = create_jc69_site_model(id = "anthus_aco")
      )
    )
  )
})
