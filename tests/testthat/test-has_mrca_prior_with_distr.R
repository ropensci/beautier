test_that("minimal use", {
  expect_silent(
    has_mrca_prior_with_distr(create_inference_model())
  )
  expect_silent(
    has_mrca_prior_with_distr(
      create_inference_model(
        mrca_prior = create_mrca_prior()
      )
    )
  )
  expect_silent(
    has_mrca_prior_with_distr(
      create_inference_model(
        mrca_prior = create_mrca_prior(
          mrca_distr = create_normal_distr()
        )
      )
    )
  )
  expect_error(has_mrca_prior_with_distr("nonsense"))
  expect_error(has_mrca_prior_with_distr(""))
  expect_error(has_mrca_prior_with_distr(NA))
  expect_error(has_mrca_prior_with_distr(NULL))
  expect_error(has_mrca_prior_with_distr(Inf))
})

test_that("use", {
  expect_false(
    has_mrca_prior_with_distr(create_inference_model())
  )
  expect_false(
    has_mrca_prior_with_distr(
      create_inference_model(
        mrca_prior = create_mrca_prior()
      )
    )
  )
  expect_true(
    has_mrca_prior_with_distr(
      create_inference_model(
        mrca_prior = create_mrca_prior(
          mrca_distr = create_normal_distr()
        )
      )
    )
  )
})
