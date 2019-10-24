context("create_inference_model")

test_that("use", {

  expect_silent(
    create_inference_model()
  )
})

test_that("has same defaults as BEAUti", {

  inference_model <- create_inference_model()

  # Site model
  expect_equal("JC69", inference_model$site_model$name)
  expect_equal("0.0", # Yes, a string, due to conversion to XML
    inference_model$site_model$gamma_site_model$prop_invariant
  )
  expect_equal("0", # Yes, a string, due to conversion to XML
    inference_model$site_model$gamma_site_model$gamma_cat_count
  )

  # Clock model
  expect_equal("strict", inference_model$clock_model$name)
  expect_equal("1.0", # Yes, a string, due to conversion to XML
    inference_model$clock_model$clock_rate_param$value
  )

  # Tree prior
  expect_equal("yule", inference_model$tree_prior$name)
  expect_equal("uniform", inference_model$tree_prior$birth_rate_distr$name)
  if (1 == 2) {
    # TODO: add lower value of minus infinity
    expect_equal(-Inf, inference_model$tree_prior$birth_rate_distr$lower)
    # TODO: add initial value of one, as a string
    expect_equal("1.0", inference_model$tree_prior$birth_rate_distr$init_value)
  }
  expect_equal(Inf, inference_model$tree_prior$birth_rate_distr$upper)

  # MCMC
  expect_equal(10000000, inference_model$mcmc$chain_length)
  expect_equal(-1, inference_model$mcmc$store_every)
  expect_equal(0, inference_model$mcmc$pre_burnin)
  expect_equal(10, inference_model$mcmc$n_init_attempts)

  # TODO: improve this test
  expect_equal("test_output_0.log", inference_model$mcmc$tracelog$filename)
  # TODO: tracelog defaults
  # TODO: screenlog defaults
  # TODO: treelog defaults
})

test_that("abuse", {

  # Most checks are done by check_inference_model
  expect_error(
    create_inference_model(site_model = "nonsense"),
    "'site_model' must be a valid site model"
  )
})
