test_that("use", {
  created <- mrca_priors_to_xml_state(
    inference_model = create_inference_model()
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})
