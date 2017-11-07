context("create_distribution")

test_that("use", {

  testthat::expect_silent(create_distribution(name = "uniform"))
})

test_that("abuse", {

  testthat::expect_error(create_distribution(name = "nonsense"))

})
