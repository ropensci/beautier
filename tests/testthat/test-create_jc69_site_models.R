context("create_jc69_site_models")

test_that("use", {

  testthat::expect_silent(create_jc69_site_models("a"))

})

test_that("create one", {

  m <- create_jc69_site_models(1)
  testthat::expect_equal(length(m), 1)
  testthat::expect_true(is_jc69_site_model(m[[1]]))

})

test_that("create two", {

  m <- create_jc69_site_models(c("a", "b"))
  testthat::expect_equal(length(m), 2)
  testthat::expect_true(is_jc69_site_model(m[[1]]))
  testthat::expect_true(is_jc69_site_model(m[[2]]))

})
