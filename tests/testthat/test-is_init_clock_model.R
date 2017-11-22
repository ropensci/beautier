context("is_init_clock_model")

test_that("strict clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rate_parameter = create_clock_rate_parameter(id = "OK")
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rate_parameter = create_clock_rate_parameter(id = NA)
      )
    )
  )

})

test_that("RLN clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = 1),
        m_parameter_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_parameter(id = NA),
          beta = create_beta_parameter(id = 1)
        ),
        m_parameter_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_parameter(id = 1),
          beta = create_beta_parameter(id = NA)
        ),
        m_parameter_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = NA),
        m_parameter_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = 1),
        m_parameter_id = NA
      )
    )
  )

})
