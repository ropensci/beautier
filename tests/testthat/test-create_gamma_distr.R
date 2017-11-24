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
      alpha = create_alphaparam(),
      beta = create_betaparam()
    )
  )

})

test_that("abuse", {


})
