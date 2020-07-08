test_that("use", {
  created <- mrca_priors_to_xml_state(
    mrca_priors = list(create_mrca_prior()),
    has_non_strict_clock_model = FALSE
  )
  expected <- NULL # Indeed, nothing
  expect_equal(created, expected)
})
