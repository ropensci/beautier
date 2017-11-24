context("is_init_clock_model")

test_that("strict clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rateparam = create_clock_rateparam(id = "OK")
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_strict_clock_model(
        clock_rateparam = create_clock_rateparam(id = NA)
      )
    )
  )

})

test_that("RLN clock model", {

  testthat::expect_true(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = 1),
        mparam_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alphaparam(id = NA),
          beta = create_betaparam(id = 1)
        ),
        mparam_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 1,
          alpha = create_alphaparam(id = 1),
          beta = create_betaparam(id = NA)
        ),
        mparam_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = NA),
        mparam_id = 2
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_clock_model(
      create_rln_clock_model(
        uclstdev_distr = create_uniform_distr(id = 1),
        mparam_id = NA
      )
    )
  )

})
