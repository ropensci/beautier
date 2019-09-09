test_that("use", {
  expect_true(is_inference_model(create_inference_model()))
  expect_false(is_inference_model(NA))
  expect_false(is_inference_model(NULL))
  expect_false(is_inference_model(Inf))
  expect_false(is_inference_model("nonsense"))
  expect_false(is_inference_model(""))

  inference_models <- c(
    list(create_inference_model()),
    list(create_inference_model())
  )
  testit::assert(length(inference_models) == 2)
  expect_false(is_inference_model(inference_models))
})
