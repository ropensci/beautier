test_that("use, no MRCA prior", {
  created <- mrca_prior_to_xml_state(
    inference_model = create_inference_model()
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})
test_that("use, no non-strict clock model", {
  created <- mrca_prior_to_xml_state(
    inference_model = create_inference_model(
      mrca_prior = create_mrca_prior(),
      clock_model = create_rln_clock_model()
    )
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})

test_that("use, non-strict clock model", {
  created <- mrca_prior_to_xml_state(
    inference_model = create_inference_model(
      mrca_prior = create_mrca_prior(),
      clock_model = create_strict_clock_model()
    )
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})

test_that("use, no non-strict clock model, mrca_distr", {
  created <- mrca_prior_to_xml_state(
    inference_model = create_inference_model(
      mrca_prior = create_mrca_prior(
        alignment_id = "test_output_0",
        mrca_distr = create_normal_distr(id = 42)
      ),
      clock_model = create_strict_clock_model()
    )
  )
  expected <- "<parameter id=\"clockRate.c:test_output_0\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expect_equal(created, expected)
  expect_match(created, "<parameter id=\"clockRate.c:")
})

test_that("use, non-strict clock model, mrca_distr", {
  created <- mrca_prior_to_xml_state(
    inference_model = create_inference_model(
      mrca_prior = create_mrca_prior(
        alignment_id = "test_output_0",
        mrca_distr = create_normal_distr(id = 42)
      ),
      clock_model = create_rln_clock_model()
    )
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})
