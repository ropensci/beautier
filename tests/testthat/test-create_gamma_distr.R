context("create_gamma_distr")

test_that("use", {

  testthat::expect_silent(
    create_gamma_distr()
  )

  testthat::expect_silent(
    create_gamma_distr(id = 1)
  )

  testthat::expect_silent(
    create_gamma_distr(
      id = 1,
      alpha = create_parameter_alpha(),
      beta = create_parameter_beta()
    )
  )

})

test_that("abuse", {


})
