test_that("use", {
  beauti_options <- create_beauti_options_v2_6_2()
  expect_true(is_beauti_options(beauti_options))
  expect_silent(check_beauti_options(beauti_options))
})
