context("are_site_models")

test_that("use", {


  testthat::expect_true(are_site_models(create_jc69_site_model(id = "a")))


  testthat::expect_true(are_site_models(create_jc69_site_models(1)))
  testthat::expect_true(are_site_models(create_jc69_site_models(2)))

  testthat::expect_false(are_site_models("nonsense"))
  testthat::expect_false(are_site_models(rep("nonsense", 2)))

  testthat::expect_false(are_site_models(NA))
  testthat::expect_false(are_site_models(NULL))

})
