test_that("use, default arguments", {

  # English
  expect_true(is_site_model(create_gtr_site_model()))
  expect_true(is_site_model(create_jc69_site_model()))
  expect_true(is_site_model(create_hky_site_model()))
  expect_true(is_site_model(create_tn93_site_model()))

  # Search-tree friendly
  expect_true(is_site_model(create_site_model_gtr()))
  expect_true(is_site_model(create_site_model_jc69()))
  expect_true(is_site_model(create_site_model_hky()))
  expect_true(is_site_model(create_site_model_tn93()))

})

test_that("abuse", {

  expect_error(
    create_site_model(
      name = "nonsense",
      id = "OK"
    ),
    "'site model' must be a site model name, which is one of these: "
  )

  expect_error(
    create_site_model(
      name = "JC69",
      id = "OK",
      gamma_site_model = "nonsense"
    ),
    "'gamma_cat_count' must be an element of a 'gamma_site_model'"
  )

})

test_that("create_gtr_site_model: devious", {

  expect_error(
    create_gtr_site_model(
      freq_param = "nonsense"
    )
  )

})

test_that("create_gtr_site_model: new interface", {

  # Old interface
  expect_silent(
    create_gtr_site_model(
      rate_ac_param = create_rate_ac_param(value = 1.2),
      rate_ag_param = create_rate_ag_param(value = 2.3),
      rate_at_param = create_rate_at_param(value = 3.4),
      rate_cg_param = create_rate_cg_param(value = 4.5),
      rate_ct_param = create_rate_ct_param(value = 5.6),
      rate_gt_param = create_rate_gt_param(value = 6.7)
    )
  )

  # New interface
  expect_silent(
    create_gtr_site_model(
      rate_ac_param = 1.2,
      rate_ag_param = 2.3,
      rate_at_param = 3.4,
      rate_cg_param = 4.5,
      rate_ct_param = 5.6,
      rate_gt_param = 6.7
    )
  )
})

test_that("create_tn93_site_model: new interface", {

  # Old interface
  expect_silent(
    create_tn93_site_model(
      kappa_1_param = create_kappa_1_param(value = 1.2),
      kappa_2_param = create_kappa_1_param(value = 2.3)
    )
  )

  # New interface
  expect_silent(
    create_tn93_site_model(
      kappa_1_param = 1.2,
      kappa_2_param = 2.3
    )
  )

})
