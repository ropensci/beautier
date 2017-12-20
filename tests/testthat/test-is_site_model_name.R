context("is_site_model_name")

test_that("basic usage", {

  testthat::expect_false(is_site_model_name("nonsense"))
  testthat::expect_true(is_site_model_name("JC69"))
  testthat::expect_true(is_site_model_name("HKY"))
  testthat::expect_true(is_site_model_name("TN93"))
  testthat::expect_true(is_site_model_name("GTR"))

})
