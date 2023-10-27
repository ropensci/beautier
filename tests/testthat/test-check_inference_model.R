test_that("use", {
  expect_silent(check_inference_model(create_inference_model()))

  # MRCA prior can be NA or initialized
  expect_silent(
    check_inference_model(
      create_inference_model(mrca_prior = NA)
    )
  )
  expect_silent(
    check_inference_model(
      create_inference_model(mrca_prior = create_mrca_prior())
    )
  )
  # tipdates_filename can be NA or a word
  expect_silent(
    check_inference_model(
      create_inference_model(tipdates_filename = NA)
    )
  )
  expect_silent(
    check_inference_model(
      create_inference_model(tipdates_filename = "my_filename.csv")
    )
  )

  # Must stop on non-MCMCs
  expect_error(check_inference_model(inference_model = "nonsense"))
  expect_error(check_inference_model(inference_model = NULL))
  expect_error(check_inference_model(inference_model = NA))
})

test_that("in-depth use", {

  good_inference_model <- create_inference_model()

  # OK
  expect_silent(
    check_inference_model(
      good_inference_model
    )
  )

  # Wrong parameter names
  inference_model <- good_inference_model
  inference_model$site_model <- NULL
  expect_error(
    check_inference_model(
      inference_model
    ),
    "'site_model' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$clock_model <- NULL
  expect_error(
    check_inference_model(
      inference_model
    ),
    "'clock_model' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$tree_prior <- NULL
  expect_error(
    check_inference_model(inference_model),
    "'tree_prior' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$mrca_prior <- NULL
  expect_error(
    check_inference_model(inference_model),
    "'mrca_prior' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$mcmc <- NULL
  expect_error(
    check_inference_model(inference_model),
    "'mcmc' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$beauti_options <- NULL
  expect_error(
    check_inference_model(inference_model),
    "'beauti_options' must be an element of an 'inference_model'"
  )

  inference_model <- good_inference_model
  inference_model$tipdates_filename <- NULL
  expect_error(
    check_inference_model(inference_model),
    "'tipdates_filename' must be an element of an 'inference_model'"
  )


  # Wrong parameter values
  expect_error(
    check_inference_model(
      create_inference_model(site_model = "nonsense")
    ),
    "'site_model' must be a valid site model"
  )
  expect_error(
    check_inference_model(
      create_inference_model(clock_model = "nonsense")
    ),
    "'clock_model' must be a valid clock model"
  )
  expect_error(
    check_inference_model(
      create_inference_model(tree_prior = "nonsense")
    ),
    "'tree_prior' must be a valid tree prior"
  )
  expect_error(
    check_inference_model(
      create_inference_model(mrca_prior = "nonsense")
    ),
    "'mrca_prior' must be a valid MRCA prior"
  )

  # Tested in-depth by check_mcmc
  expect_error(
    check_inference_model(
      create_inference_model(mcmc = "nonsense")
    ),
    "mcmc"
  )

  expect_error(
    check_inference_model(
      create_inference_model(beauti_options = "nonsense")
    ),
    "'beauti_options' must be a valid BEAUti options"
  )
  expect_error(
    check_inference_model(
      create_inference_model(tipdates_filename = NULL)
    ),
    "must be a single string"
  )
  expect_error(
    check_inference_model(
      create_inference_model(tipdates_filename = c("nons", "ense"))
    ),
    "must be a single string"
  )
  expect_error(
    check_inference_model(
      create_inference_model(tipdates_filename = 1)
    ),
    "must be a single string or `NA`, not the number 1"
  )
})
