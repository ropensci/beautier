test_that("use, MCMC store every of 2k", {
  if (!beastier::is_beast2_installed()) return()

  inference_model <- create_inference_model(
    mcmc = create_mcmc(chain_length = 6000, store_every = 2000)
  )
  text <- create_beast2_input_from_model(
    input_filename = get_fasta_filename(),
    inference_model = inference_model
  )
  store_every <- as.numeric(
    na.omit(
      stringr::str_match(
        string = text,
        pattern = ".*MCMC.*storeEvery=.(.*).>"
      )[, 2]
    )
  )
  expect_equal(
    inference_model$mcmc$store_every,
    store_every
  )

  skip("Expose #94")

  log_every <- as.numeric(
    na.omit(
      stringr::str_match(
        string = text,
        pattern = "treelog.*logEvery..(.*). "
      )[, 2]
    )
  )
  expect_equal(
    inference_model$mcmc$store_every,
    log_every
  )
})
