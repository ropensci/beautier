context("is_init_site_model")

test_that("is_init_site_model", {

  expect_false(is_init_site_model("nonsense"))
  expect_false(is_init_gtr_site_model("nonsense"))
})

test_that("is_init_gtr_site_model", {

  g <- create_gtr_site_model(
    id = 42,
    gamma_site_model = create_gamma_site_model(
      gamma_cat_count = 2,
      gamma_shape_prior_distr = create_one_div_x_distr(id = 0)
    ),
    rate_ac_prior_distr = create_one_div_x_distr(id = 1),
    rate_ag_prior_distr = create_one_div_x_distr(id = 2),
    rate_at_prior_distr = create_one_div_x_distr(id = 3),
    rate_cg_prior_distr = create_one_div_x_distr(id = 4),
    rate_gt_prior_distr = create_one_div_x_distr(id = 6),
    rate_ac_param = create_rate_ac_param(id = 7),
    rate_ag_param = create_rate_ac_param(id = 8),
    rate_at_param = create_rate_ac_param(id = 9),
    rate_cg_param = create_rate_ac_param(id = 10),
    rate_ct_param = create_rate_ac_param(id = 11),
    rate_gt_param = create_rate_ac_param(id = 12)
  )
  testit::assert(is_init_gtr_site_model(g))

  # Uninitialize 'rate_ac_prior_distr'
  h <- g
  h$rate_ac_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_ag_prior_distr'
  h <- g
  h$rate_ag_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_at_prior_distr'
  h <- g
  h$rate_at_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_cg_prior_distr'
  h <- g
  h$rate_cg_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # No 'rate_cg_prior_distr' yet

  # Uninitialize 'rate_gt_prior_distr'
  h <- g
  h$rate_gt_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_ac_param'
  h <- g
  h$rate_ac_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_ag_param'
  h <- g
  h$rate_ag_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_at_param'
  h <- g
  h$rate_at_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_cg_param'
  h <- g
  h$rate_cg_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_ct_param'
  h <- g
  h$rate_ct_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'rate_gt_param'
  h <- g
  h$rate_gt_param$id <- NA
  expect_false(is_init_gtr_site_model(h))

  # Uninitialize 'gamma_site_model'
  h <- g
  h$gamma_site_model$gamma_shape_prior_distr$id <- NA
  expect_false(is_init_gtr_site_model(h))
})


test_that("is_init_site_model on HKY site model", {

  expect_true(
    is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
          m = create_m_param(id = 1),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  expect_false(
    is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(
          id = NA,
          m = create_m_param(id = 1),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  expect_false(
    is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(
          id = 1,
          m = create_m_param(id = NA),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  expect_false(
    is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(
          id = 1,
          m = create_m_param(id = 1),
          s = create_s_param(id = NA)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape_prior_distr = create_one_div_x_distr(id = 3)
        )
      )
    )
  )

  expect_false(
    is_init_site_model(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(id = 1,
          m = create_m_param(id = 1),
          s = create_s_param(id = 2)
        ),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
        )
      )
    )
  )

})
