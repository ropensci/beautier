test_that("use", {
  expect_true(
    has_rln_clock_model(
      create_inference_model(clock_model = create_rln_clock_model())
    )
  )
  expect_false(
    has_rln_clock_model(
      create_inference_model(clock_model = create_strict_clock_model())
    )
  )
  expect_error(
    has_rln_clock_model("nonsense"),
    "inference_model"
  )
})
