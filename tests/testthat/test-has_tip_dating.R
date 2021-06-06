test_that("use", {
  expect_true(
    has_tip_dating(
      create_inference_model(
        tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
      )
    )
  )
  expect_false(
    has_tip_dating(
      create_inference_model()
    )
  )
  expect_error(
    has_tip_dating("nonsense"),
    "inference_model"
  )
})
