context("init_site_models")

test_that("initialize HKY site model", {

  id <- "a"
  before <- list(create_hky_site_model())
  testit::assert(!beautier:::are_init_site_models(before))
  after <- beautier:::init_site_models(before, ids = id)
  testthat::expect_true(beautier:::are_init_site_models(after))

})
