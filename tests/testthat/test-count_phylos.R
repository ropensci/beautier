context("count_phylos")

test_that("use", {

  skip("Obsoleted")
  testthat::expect_equal(count_phylos(NULL), 0)
  testthat::expect_equal(count_phylos(ape::rcoal(3)), 1)
  testthat::expect_equal(count_phylos(rep(ape::rcoal(3), 2)), 2)
  testthat::expect_equal(count_phylos(c()), 0)

})

test_that("abuse", {

  # testthat::expect_error(count_phylos(NA))
  testthat::expect_error(count_phylos("nonsense"))
  testthat::expect_error(count_phylos(c("nonsense")))
  testthat::expect_error(count_phylos(c("nonsense", "too")))
  testthat::expect_error(count_phylos(c("nonsense", "too", "too")))

  skip("WIP: cannot extract phylos yet")
  testthat::expect_error(count_phylos(c("nonsense", "too", "too", "too")))

})
