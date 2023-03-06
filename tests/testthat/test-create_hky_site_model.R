test_that("use", {
  skip("Issue #136, Issue 136")
  hky_site_model <- create_hky_site_model()
  expect_false("kappa" %in% names(hky_site_model))
  expect_true("kappa_param" %in% names(hky_site_model))
})
