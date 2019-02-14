context("test-has_mrca_prior")

test_that("use", {
  expect_false(has_mrca_prior(create_inference_model()))

  expect_true(
    has_mrca_prior(
      create_inference_model(
        mrca_prior = create_mrca_prior()
      )
    )
  )
})
