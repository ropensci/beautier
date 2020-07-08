test_that("use, no MRCA prior", {
  created <- mrca_prior_to_xml_state(
    mrca_prior = NA,
    has_non_strict_clock_model = "irrelevant"
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})
test_that("use, no non-strict clock model", {
  created <- mrca_prior_to_xml_state(
    mrca_prior = create_mrca_prior(),
    has_non_strict_clock_model = FALSE
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})

test_that("use, non-strict clock model", {
  created <- mrca_prior_to_xml_state(
    mrca_prior = create_mrca_prior(),
    has_non_strict_clock_model = TRUE
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})

test_that("use, no non-strict clock model, mrca_distr", {
  created <- mrca_prior_to_xml_state(
    mrca_prior = create_mrca_prior(
      alignment_id = "test_output_0",
      mrca_distr = create_normal_distr(id = 42)
    ),
    has_non_strict_clock_model = FALSE
  )
  expected <- "<parameter id=\"clockRate.c:test_output_0\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expect_equal(created, expected)
  expect_match(created, "<parameter id=\"clockRate.c:")
})

test_that("use, non-strict clock model, mrca_distr", {
  created <- mrca_prior_to_xml_state(
    mrca_prior = create_mrca_prior(
      alignment_id = "test_output_0",
      mrca_distr = create_normal_distr(id = 42)
    ),
    has_non_strict_clock_model = TRUE
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})