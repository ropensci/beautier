test_that("use", {
  inference_model <- create_inference_model()
  expect_false(needs_trait_set_str(inference_model))

  inference_model <- create_inference_model(
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
  )
  expect_false(needs_trait_set_str(inference_model))

  inference_model <- create_inference_model(
    tipdates_filename = get_beautier_path("babette_issue_109.tsv")
  )
  expect_true(needs_trait_set_str(inference_model))
})
