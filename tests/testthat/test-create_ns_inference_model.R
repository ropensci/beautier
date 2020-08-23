test_that("use", {
  inference_model <- create_ns_inference_model()
  expect_true(beautier::is_nested_sampling_mcmc(inference_model$mcmc))
})
