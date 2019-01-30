context("create_inference_model")

test_that("use", {

  expect_silent(
    create_inference_model()
  )
})

test_that("abuse", {

  # Most checks are done by check_inference_model
  expect_error(
    create_inference_model(site_model = "nonsense"),
    "'site_model' must be a valid site model"
  )
})
