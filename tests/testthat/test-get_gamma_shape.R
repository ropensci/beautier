context("get_gamma_shape")

test_that("use", {

  testthat::expect_equal(
    get_gamma_shape(create_gamma_site_model(gamma_shape = 42)),
    42
  )

  testthat::expect_equal(
    get_gamma_shape(
      create_gamma_site_model()
    ),
    get_default_gamma_shape()
  )

})


test_that("abuse", {

  testthat::expect_error(get_gamma_shape("nonsense"))

})
