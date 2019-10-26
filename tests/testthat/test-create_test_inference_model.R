test_that("use", {
  inference_model <- create_test_inference_model()
  expect_equal(inference_model$mcmc$chain_length, 3000)
  expect_equal(inference_model$mcmc$store_every, 1000)
  expect_true(!file.exists(inference_model$mcmc$tracelog$filename))
  expect_true(!file.exists(inference_model$mcmc$screenlog$filename))
  expect_true(!file.exists(inference_model$mcmc$treelog$filename))
})
