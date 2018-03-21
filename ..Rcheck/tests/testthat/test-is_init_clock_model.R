context("is_init_clock_model")

test_that("strict clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rate_param = create_clock_rate_param(id = "OK"),
        clock_rate_distr = create_uniform_distr(id = "OK")
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rate_param = create_clock_rate_param(id = NA),
        clock_rate_distr = create_uniform_distr(id = "OK")
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rate_param = create_clock_rate_param(id = "OK"),
        clock_rate_distr = create_uniform_distr(id = NA)
      )
    )
  )
})

test_that("RLN clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        mean_rate_prior_distr = create_uniform_distr(id = 1),
        ucldstdev_distr = create_uniform_distr(id = 2),
        mparam_id = 2,
        dimension = 42
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_param(id = NA),
          beta = create_beta_param(id = 1)
        ),
        mparam_id = 2,
        dimension = 42
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_param(id = 1),
          beta = create_beta_param(id = NA)
        ),
        mparam_id = 2,
        dimension = 42
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        ucldstdev_distr = create_uniform_distr(id = NA),
        mparam_id = 2,
        dimension = 42
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        ucldstdev_distr = create_uniform_distr(id = 1),
        mparam_id = NA,
        dimension = 42
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        ucldstdev_distr = create_uniform_distr(id = 1),
        mparam_id = 2,
        dimension = NA
      )
    )
  )

})
