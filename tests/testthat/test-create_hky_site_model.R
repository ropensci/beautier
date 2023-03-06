test_that("use", {
  hky_site_model <- beautier::create_hky_site_model()
  expect_false("kappa" %in% names(hky_site_model))
  expect_true("kappa_param" %in% names(hky_site_model))
})

test_that("obsolete", {
  expect_error(beautier::create_hky_site_model(kappa = "sometying"))
})
