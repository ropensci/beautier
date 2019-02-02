context("test-check_inference_models")


test_that("use", {
  expect_silent(check_inference_models(list(create_inference_model())))

  expect_error(
    check_inference_models(list(create_inference_model(), "nonsense"))
  )
  expect_error(check_inference_models(create_inference_model()))
  expect_error(check_inference_models("nonsense"))
  expect_error(check_inference_models(NULL))
  expect_error(check_inference_models(NA))
})
