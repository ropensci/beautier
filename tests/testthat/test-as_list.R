context("as_list")

test_that("example", {

  v <- as_list(create_hky_site_model(), create_jc69_site_model())
  testit::assert(is_hky_site_model(v[[1]]))
  testit::assert(is_jc69_site_model(v[[2]]))

})
