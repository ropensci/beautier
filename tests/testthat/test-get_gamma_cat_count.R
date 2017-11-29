context("get_gamma_cat_count")

test_that("use", {

  testthat::expect_equal(
    get_gamma_cat_count(create_gamma_site_model(gamma_cat_count = 42)),
    42
  )

  testthat::expect_equal(
    get_gamma_cat_count(
      create_gamma_site_model()
    ),
    get_default_gamma_cat_count()
  )

})


test_that("abuse", {

  testthat::expect_error(get_gamma_cat_count("nonsense"))

})
