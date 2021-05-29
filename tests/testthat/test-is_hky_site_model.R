test_that("use", {
  expect_true(is_hky_site_model(create_hky_site_model()))
  expect_false(is_hky_site_model(create_jc69_site_model()))
})
