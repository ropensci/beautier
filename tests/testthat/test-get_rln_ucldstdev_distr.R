context("get_rln_ucldstdev_distr")

test_that("use", {

  testthat::expect_silent(
    get_rln_ucldstdev_distr(
      create_rln_clock_model()
    )
  )

  testthat::expect_silent(
    get_rln_ucldstdev_distr(
      create_rln_clock_model(
        ucldstdev_distr = create_gamma_distr(id = 1)
      )
    )
  )

})

test_that("abuse", {

  testthat::expect_error(get_rln_ucldstdev_distr("nonsense"))
  testthat::expect_error(get_rln_ucldstdev_distr(NULL))
  testthat::expect_error(get_rln_ucldstdev_distr(NA))

})
