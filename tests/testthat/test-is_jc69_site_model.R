context("is_jc69_site_model")

test_that("use", {

  testthat::expect_true(is_jc69_site_model(create_site_model(name = "JC69")))
  testthat::expect_false(is_jc69_site_model(create_site_model(name = "HKY")))

})
