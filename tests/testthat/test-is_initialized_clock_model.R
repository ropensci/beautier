context("is_initialized_clock_model")

test_that("use", {


  testthat::expect_true(
    beautier:::is_initialized_clock_model(
      create_strict_clock_model(
        clock_rate_parameter = create_clock_rate_parameter(id = "OK")
      )
    )
  )

  testthat::expect_false(
    beautier:::is_initialized_clock_model(
      create_strict_clock_model(
        clock_rate_parameter = create_clock_rate_parameter(id = NA)
      )
    )
  )


  testthat::expect_true(
    beautier:::is_initialized_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_false(
    beautier:::is_initialized_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_parameter(id = NA),
          beta = create_beta_parameter(id = 1)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_initialized_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_parameter(id = 1),
          beta = create_beta_parameter(id = NA)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_initialized_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = NA)
      )
    )
  )

})
