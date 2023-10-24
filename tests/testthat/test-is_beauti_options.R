context("is_beauti_options")

test_that("use", {

  expect_true(is_beauti_options(create_beauti_options()))

})

test_that("use, devious", {

  g <- create_beauti_options()
  expect_true(is_beauti_options(g))

  # No 'capitalize_first_char_id'
  h <- g[names(g) != "capitalize_first_char_id"]
  expect_false(is_beauti_options(h))

  # No 'nucleotides_uppercase'
  h <- g[names(g) != "nucleotides_uppercase"]
  expect_false(is_beauti_options(h))

  # No 'sequence_indent'
  h <- g[names(g) != "sequence_indent"]
  expect_false(is_beauti_options(h))
})
