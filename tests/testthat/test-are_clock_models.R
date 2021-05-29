test_that("use", {
  expect_true(are_clock_models(create_strict_clock_model()))
  expect_false(are_clock_models("nonsense"))
  expect_false(are_clock_models(rep("nonsense", 2)))
  expect_false(are_clock_models(NA))
  expect_false(are_clock_models(NULL))
})
