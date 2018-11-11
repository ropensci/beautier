context("are_site_models")

test_that("use", {

  expect_true(are_site_models(create_jc69_site_model(id = "a")))
  expect_false(are_site_models("nonsense"))
  expect_false(are_site_models(rep("nonsense", 2)))
  expect_false(are_site_models(NA))
  expect_false(are_site_models(NULL))

})
