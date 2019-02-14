context("test-has_mrca_prior")

test_that("use, no MRCA prior", {
  expect_false(has_mrca_prior(create_inference_model()))
})
