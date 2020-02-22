test_that("mcmc$pre_burning", {

  inference_model <- create_inference_model(
    mcmc = create_mcmc(
      chain_length = 1e7,
      store_every = 1e4,
      pre_burnin = 1e6
    )
  )
  text <- create_beast2_input_from_model(
    input_filename = get_fasta_filename(),
    inference_model = inference_model
  )
  the_line <- as.character(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*MCMC.*"
      )[, 1]
    )
  )
  testit::assert(nchar(the_line) > 0)
  pre_burnin <- as.numeric(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*preBurnin=.(.*). "
      )[, 2]
    )
  )
  expect_equal(
    inference_model$mcmc$pre_burnin,
    pre_burnin
  )


})


test_that("mcmc$n_init_attempts", {

  inference_model <- create_inference_model(
    mcmc = create_mcmc(
      n_init_attempts = 314
    )
  )
  text <- create_beast2_input_from_model(
    input_filename = get_fasta_filename(),
    inference_model = inference_model
  )
  the_line <- as.character(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*MCMC.*"
      )[, 1]
    )
  )
  the_line
  testit::assert(nchar(the_line) > 0)
  n_init_attempts <- as.numeric(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*numInitializationAttempts=.(.*).."
      )[, 2]
    )
  )
  expect_equal(
    inference_model$mcmc$n_init_attempts,
    n_init_attempts
  )
})

test_that("mcmc$sample_from_prior", {

  inference_model <- create_inference_model(
    mcmc = create_mcmc(
      sample_from_prior = TRUE
    )
  )
  text <- create_beast2_input_from_model(
    input_filename = get_fasta_filename(),
    inference_model = inference_model
  )
  the_line <- as.character(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*MCMC.*"
      )[, 1]
    )
  )
  the_line
  testit::assert(nchar(the_line) > 0)
  sample_from_prior <- as.logical(
    stats::na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*sampleFromPrior=.(.*).."
      )[, 2]
    )
  )
  expect_equal(
    inference_model$mcmc$sample_from_prior,
    sample_from_prior
  )
})
