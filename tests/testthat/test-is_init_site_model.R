context("is_init_site_model")

test_that("hky site model", {

  testthat::expect_true(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
          m = create_m_param(id = 1),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = NA,
            m = create_m_param(id = 1),
            s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
            m = create_m_param(id = NA),
            s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
            m = create_m_param(id = 1),
            s = create_s_param(id = NA)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
          m = create_m_param(id = 1),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
        )
      )
    )
  )

})
