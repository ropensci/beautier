context("create_distr")

test_that("use", {

  testthat::expect_silent(create_distr(name = "uniform", id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1, upper = 1000))
})

test_that("abuse", {

  testthat::expect_error(create_distr(name = "nonsense"))

})
