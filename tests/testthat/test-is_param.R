test_that("use", {

  expect_true(is_param(create_alpha_param()))
  expect_true(is_param(create_beta_param()))
  expect_true(is_param(create_clock_rate_param()))
  expect_true(is_param(create_freq_param()))
  expect_true(is_param(create_m_param()))
  expect_true(is_param(create_mean_param()))
  expect_true(is_param(create_mu_param()))
  expect_true(is_param(create_kappa_param()))
  expect_true(is_param(create_kappa_1_param()))
  expect_true(is_param(create_kappa_2_param()))
  expect_true(is_param(create_s_param()))
  expect_true(is_param(create_scale_param()))
  expect_true(is_param(create_sigma_param()))

  expect_false(is_param(NULL))
  expect_false(is_param(NA))
  expect_false(is_param(c()))
  expect_false(is_param(ape::rcoal(3)))

})

test_that("is_param, devious", {

  g <- create_alpha_param()
  expect_true(is_param(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_param(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_param(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_param(h))

  # No 'value'
  h <- g[names(g) != "value"]
  expect_false(is_param(h))

  # 'value' is NA
  h <- g
  h$value <- NA
  expect_false(is_param(h))
})

test_that("is_alpha_param", {

  expect_true(is_alpha_param(create_alpha_param()))
  expect_false(is_alpha_param("nonsense"))

})


test_that("is_b_pop_sizes_param", {

  expect_true(is_b_pop_sizes_param(create_b_pop_sizes_param()))
  expect_false(is_b_pop_sizes_param("nonsense"))

})


test_that("is_beta_param", {

  expect_true(is_beta_param(create_beta_param()))
  expect_false(is_beta_param("nonsense"))

})

test_that("is_clock_rate_param", {

  expect_true(is_clock_rate_param(create_clock_rate_param()))
  expect_false(is_clock_rate_param("nonsense"))

})

test_that("is_freq_param", {

  expect_true(is_freq_param(create_freq_param()))
  expect_false(is_freq_param("nonsense"))
  expect_false(is_freq_param(create_b_pop_sizes_param()))
})

test_that("is_freq_param, devious", {

  g <- create_freq_param()
  expect_true(is_freq_param(g))

  # No 'lower'
  h <- g[names(g) != "lower"]
  expect_false(is_freq_param(h))

  # No 'upper'
  h <- g[names(g) != "upper"]
  expect_false(is_freq_param(h))

  # No 'value'
  h <- g[names(g) != "value"]
  expect_false(is_freq_param(h))

  # No 'estimate'
  h <- g[names(g) != "estimate"]
  expect_false(is_freq_param(h))

  # No 'dimension'
  h <- g[names(g) != "dimension"]
  expect_false(is_freq_param(h))
})

test_that("is_kappa_param", {

  expect_true(is_kappa_param(create_kappa_param()))
  expect_false(is_kappa_param("nonsense"))
})

test_that("is_kappa_param, devious", {

  g <- create_kappa_param()
  expect_true(is_kappa_param(g))

  # No 'lower'
  h <- g[names(g) != "lower"]
  expect_false(is_kappa_param(h))

  # No 'estimate'
  h <- g[names(g) != "estimate"]
  expect_false(is_kappa_param(h))
})

test_that("is_kappa_1_param", {

  expect_true(is_kappa_1_param(create_kappa_1_param()))
  expect_false(is_kappa_1_param("nonsense"))
})

test_that("is_kappa_1_param, devious", {

  g <- create_kappa_1_param()
  expect_true(is_kappa_1_param(g))

  # No 'lower'
  h <- g[names(g) != "lower"]
  expect_false(is_kappa_1_param(h))

  # No 'estimate'
  h <- g[names(g) != "estimate"]
  expect_false(is_kappa_1_param(h))
})

test_that("is_kappa_2_param", {

  expect_true(is_kappa_2_param(create_kappa_2_param()))
  expect_false(is_kappa_2_param("nonsense"))

})

test_that("is_kappa_2_param, devious", {

  g <- create_kappa_2_param()
  expect_true(is_kappa_2_param(g))

  # No 'lower'
  h <- g[names(g) != "lower"]
  expect_false(is_kappa_2_param(h))

  # No 'estimate'
  h <- g[names(g) != "estimate"]
  expect_false(is_kappa_2_param(h))
})

test_that("is_lambda_param", {

  expect_true(is_lambda_param(create_lambda_param()))
  expect_false(is_lambda_param("nonsense"))

})

test_that("is_mean_param", {
  expect_true(is_mean_param(create_mean_param()))
  expect_false(is_mean_param("nonsense"))
})

test_that("is_mu_param", {

  expect_true(is_mu_param(create_mu_param()))
  expect_false(is_mu_param("nonsense"))

})

test_that("is_rate_ac_param", {

  expect_true(is_rate_ac_param(create_rate_ac_param()))
  expect_false(is_rate_ac_param("nonsense"))

})

test_that("is_rate_ag_param", {

  expect_true(is_rate_ag_param(create_rate_ag_param()))
  expect_false(is_rate_ag_param("nonsense"))

})

test_that("is_rate_at_param", {

  expect_true(is_rate_at_param(create_rate_at_param()))
  expect_false(is_rate_at_param("nonsense"))

})

test_that("is_rate_cg_param", {

  expect_true(is_rate_cg_param(create_rate_cg_param()))
  expect_false(is_rate_cg_param("nonsense"))

})

test_that("is_rate_ct_param", {

  expect_true(is_rate_ct_param(create_rate_ct_param()))
  expect_false(is_rate_ct_param("nonsense"))

})

test_that("is_rate_gt_param", {

  expect_true(is_rate_gt_param(create_rate_gt_param()))
  expect_false(is_rate_gt_param("nonsense"))

})

test_that("is_s_param", {

  expect_true(is_s_param(create_s_param()))
  expect_false(is_s_param("nonsense"))

})

test_that("is_scale_param", {

  expect_true(is_scale_param(create_scale_param()))
  expect_false(is_scale_param("nonsense"))

})

test_that("is_sigma_param", {

  expect_true(is_sigma_param(create_sigma_param()))
  expect_false(is_sigma_param("nonsense"))

})
