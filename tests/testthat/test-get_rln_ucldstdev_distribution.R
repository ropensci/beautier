context("get_rln_ucldstdev_distribution")

test_that("use", {

  testthat::expect_silent(
    get_rln_ucldstdev_distribution(
      create_rln_clock_model()
    )
  )

  testthat::expect_silent(
    get_rln_ucldstdev_distribution(
      create_rln_clock_model(
        uclstdev_distribution = create_gamma_distr(id = 1)
      )
    )
  )

})

test_that("abuse", {

  testthat::expect_error(get_rln_ucldstdev_distribution("nonsense"))
  testthat::expect_error(get_rln_ucldstdev_distribution(NULL))
  testthat::expect_error(get_rln_ucldstdev_distribution(NA))

})
