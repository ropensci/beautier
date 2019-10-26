context("test-check_mcmc")

test_that("use", {
  expect_silent(check_mcmc(create_mcmc()))
  expect_silent(check_mcmc(create_ns_mcmc()))

  # Must stop on non-MCMCs
  expect_error(check_mcmc(mcmc = "nonsense"))
  expect_error(check_mcmc(mcmc = NULL))
  expect_error(check_mcmc(mcmc = NA))

  expect_silent(
    check_mcmc(
      create_mcmc(
        chain_length = 1e7
      )
    )
  )
  expect_silent(
    check_mcmc(
      create_mcmc(
        store_every = 1e3
      )
    )
  )

  expect_silent(
    check_mcmc(
      create_mcmc(
        pre_burnin = 1e6
      )
    )
  )
  expect_silent(
    check_mcmc(
      create_mcmc(
        n_init_attempts = 3
      )
    )
  )

  expect_silent(
    check_mcmc(
      create_mcmc(
        sample_from_prior = TRUE
      )
    )
  )

  expect_silent(
    check_mcmc(
      create_mcmc(
        treelog = create_treelog()
      )
    )
  )
  expect_silent(
    check_mcmc(
      create_mcmc(
        screenlog = create_screenlog()
      )
    )
  )
  expect_silent(
    check_mcmc(
      create_mcmc(
        tracelog = create_tracelog()
      )
    )
  )

})

test_that("missing list elements", {

  good_mcmc <- create_mcmc()

  # OK
  expect_silent(
    check_mcmc(
      good_mcmc
    )
  )

  mcmc <- good_mcmc
  mcmc$chain_length <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'chain_length' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$store_every <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'store_every' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$pre_burnin <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'pre_burnin' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$n_init_attempts <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'n_init_attempts' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$sample_from_prior <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'sample_from_prior' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$treelog <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'treelog' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$screenlog <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'screenlog' must be an element of an 'mcmc'"
  )

  mcmc <- good_mcmc
  mcmc$tracelog <- NULL
  expect_error(
    check_mcmc(
      mcmc
    ),
    "'tracelog' must be an element of an 'mcmc'"
  )
})

test_that("invalid list element values", {

  expect_error(
    check_mcmc(
      create_mcmc(
        chain_length = -12345
      )
    ),
    "'mcmc.chain_length' must be non-zero and positive"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        store_every = -12345
      )
    ),
    "'mcmc.store_every' must be either -1 or a non-zero positive value"
  )
  expect_error(
    check_mcmc(
      create_mcmc(
        store_every = 0
      )
    ),
    "'mcmc.store_every' must be either -1 or a non-zero positive value"
  )
  expect_error(
    check_mcmc(
      create_mcmc(
        chain_length = 1000,
        store_every = 1000000
      )
    ),
    "'mcmc.store_every' must be less than 'mcmc.chain_length'"
  )
  expect_error(
    check_mcmc(
      create_mcmc(
        store_every = 1
      )
    ),
    "'mcmc.store_every' must be at least 1000, NA or -1"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        chain_length = 1e7,
        store_every = 1e3,
        pre_burnin = 1e123
      )
    ),
    "'mcmc.pre_burnin' must be less than 'mcmc.chain_length'"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        n_init_attempts = -1234
      )
    ),
    "mcmc.n_init_attempts"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        sample_from_prior = -1234
      )
    ),
    "mcmc.sample_from_prior"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        treelog = "nonsense"
      )
    ),
    "treelog"
  )

  expect_error(
    check_mcmc(
      create_mcmc(
        screenlog = "nonsense"
      )
    ),
    "screenlog"
  )
  expect_error(
    check_mcmc(
      create_mcmc(
        tracelog = "nonsense"
      )
    ),
    "tracelog"
  )
})
