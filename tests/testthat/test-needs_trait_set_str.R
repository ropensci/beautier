test_that("use", {
  skip("WIP")
  inference_model <- create_inference_model()
  expect_false(needs_trait_set_str(inference_model))

  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_true(needs_trait_set_str(inference_model))

  inference_model <- create_inference_model(
    tree_prior = create_cbs_tree_prior(),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_true(needs_trait_set_str(inference_model))
})


