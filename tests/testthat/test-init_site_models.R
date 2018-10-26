context("init_site_models")

test_that("initialize JC69 site model", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2,
    gamma_shape_prior_distr = create_exp_distr()
  )

  id <- "a"
  before <- list(create_jc69_site_model(gamma_site_model = gamma_site_model))
  testit::assert(is_jc69_site_model(before[[1]]))
  testit::assert(!are_init_site_models(before))
  after <- init_site_models(before, ids = id)
  testit::assert(is_jc69_site_model(after[[1]]))
  expect_true(are_init_site_models(after))

})

test_that("initialize HKY site model", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2,
    gamma_shape_prior_distr = create_exp_distr()
  )

  id <- "a"
  before <- list(create_hky_site_model(gamma_site_model = gamma_site_model))
  testit::assert(is_hky_site_model(before[[1]]))
  testit::assert(!are_init_site_models(before))
  after <- init_site_models(before, ids = id)
  testit::assert(is_hky_site_model(after[[1]]))
  expect_true(are_init_site_models(after))

})

test_that("initialize TN93 site model", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2,
    gamma_shape_prior_distr = create_exp_distr()
  )

  id <- "a"
  before <- list(create_tn93_site_model(gamma_site_model = gamma_site_model))
  testit::assert(is_tn93_site_model(before[[1]]))
  testit::assert(!are_init_site_models(before))
  after <- init_site_models(before, ids = id)
  testit::assert(is_tn93_site_model(after[[1]]))
  expect_true(are_init_site_models(after))

})

test_that("initialize GTR site model", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2,
    gamma_shape_prior_distr = create_exp_distr()
  )

  id <- "a"
  before <- list(create_gtr_site_model(gamma_site_model = gamma_site_model))
  testit::assert(is_gtr_site_model(before[[1]]))
  testit::assert(!are_init_site_models(before))
  after <- init_site_models(before, ids = id)
  testit::assert(is_gtr_site_model(after[[1]]))
  expect_true(are_init_site_models(after))

})
