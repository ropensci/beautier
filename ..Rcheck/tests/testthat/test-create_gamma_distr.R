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
      alpha = create_alpha_param(),
      beta = create_beta_param()
    )
  )

})

test_that("abuse", {


})
