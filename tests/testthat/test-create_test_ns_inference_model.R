test_that("use", {

  inference_model <- create_test_ns_inference_model()
  expect_true(beautier::is_inference_model(inference_model))
  expect_true(beautier::is_nested_sampling_mcmc(inference_model$mcmc))
})
