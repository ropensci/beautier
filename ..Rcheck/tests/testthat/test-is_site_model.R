context("is_site_model")

test_that("use", {

  testthat::expect_true(is_site_model(create_jc69_site_model()))

  testthat::expect_false(is_site_model(NA))
  testthat::expect_false(is_site_model(NULL))
  testthat::expect_false(is_site_model("nonsense"))
  testthat::expect_false(is_site_model(list(name = "nonsense")))
  testthat::expect_false(is_site_model(list(name = "JC69")))

})
