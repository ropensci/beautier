test_that("warning", {

  inference_model <- create_inference_model(
    site_model = create_jc69_site_model(id = 123)
  )
  expect_warning(
    site_model_to_xml_subst_model(
      inference_model = inference_model
    ),
    "'site_model_to_xml_subst_model' is deprecated"
  )
})

test_that("deprecated", {

  expect_error(
    site_model_to_xml_subst_model(
      site_model = "something",
      inference_model = "irrelevant"
    ),
    "'site_model' is deprecated, use 'inference_model' instead"
  )
})
