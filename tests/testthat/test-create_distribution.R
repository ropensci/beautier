context("create_distribution")

test_that("use", {

  testthat::expect_silent(create_distribution(name = "uniform", id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1, upper = 1000))
})

test_that("abuse", {

  testthat::expect_error(create_distribution(name = "nonsense"))

})
