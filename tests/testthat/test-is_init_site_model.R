context("is_init_site_model")

test_that("hky site model", {

  testthat::expect_true(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior = create_log_normal_distr(id = 1,
          m = create_m_parameter(id = 1),
          s = create_s_parameter(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior = create_log_normal_distr(id = NA,
            m = create_m_parameter(id = 1),
            s = create_s_parameter(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior = create_log_normal_distr(id = 1,
            m = create_m_parameter(id = NA),
            s = create_s_parameter(id = 2)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior = create_log_normal_distr(id = 1,
            m = create_m_parameter(id = 1),
            s = create_s_parameter(id = NA)
        )
      )
    )
  )

})
