test_that("use", {
  beauti_options <- create_beauti_options_v2_4()
  expect_silent(check_beauti_options(beauti_options))
  expect_equal(beauti_options$capitalize_first_char_id, FALSE)
  expect_equal(beauti_options$nucleotides_uppercase, FALSE)
  expect_equal(beauti_options$beast2_version, "2.4")
  expect_equal(beauti_options$required, "")
  expect_equal(beauti_options$sequence_indent, 20)
})
