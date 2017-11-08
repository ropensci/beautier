context("create_distribution")

test_that("use", {

  testthat::expect_silent(create_distribution(name = "uniform", id = 1))
})

test_that("abuse", {

  testthat::expect_error(create_distribution(name = "nonsense"))

})
