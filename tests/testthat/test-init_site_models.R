context("init_site_models")

test_that("initialize JC69 site model", {

  id <- "a"
  before <- list(create_jc69_site_model())
  testit::assert(beautier:::is_jc69_site_model(before[[1]]))
  testit::assert(!beautier:::are_init_site_models(before))
  after <- beautier:::init_site_models(before, ids = id)
  testit::assert(beautier:::is_jc69_site_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_site_models(after))

})

test_that("initialize HKY site model", {

  id <- "a"
  before <- list(create_hky_site_model())
  testit::assert(beautier:::is_hky_site_model(before[[1]]))
  testit::assert(!beautier:::are_init_site_models(before))
  after <- beautier:::init_site_models(before, ids = id)
  testit::assert(beautier:::is_hky_site_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_site_models(after))

})

test_that("initialize TN93 site model", {

  id <- "a"
  before <- list(create_tn93_site_model())
  testit::assert(beautier:::is_tn93_site_model(before[[1]]))
  testit::assert(!beautier:::are_init_site_models(before))
  after <- beautier:::init_site_models(before, ids = id)
  testit::assert(beautier:::is_tn93_site_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_site_models(after))

})

test_that("initialize GTR site model", {

  id <- "a"
  before <- list(create_gtr_site_model())
  testit::assert(beautier:::is_gtr_site_model(before[[1]]))
  testit::assert(!beautier:::are_init_site_models(before))
  after <- beautier:::init_site_models(before, ids = id)
  testit::assert(beautier:::is_gtr_site_model(after[[1]]))
  testthat::expect_true(beautier:::are_init_site_models(after))

})
