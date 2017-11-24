context("is_init_site_model")

test_that("hky site model", {

  testthat::expect_true(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
          m = create_mparam(id = 1),
          s = create_sparam(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = NA,
            m = create_mparam(id = 1),
            s = create_sparam(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
            m = create_mparam(id = NA),
            s = create_sparam(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
            m = create_mparam(id = 1),
            s = create_sparam(id = NA)
        )
      )
    )
  )

})
