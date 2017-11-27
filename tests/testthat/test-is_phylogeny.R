context("is_phylogeny")

test_that("is_phylogeny: use", {

  testthat::expect_true(is_phylogeny(ape::rcoal(n = 5)))
  testthat::expect_false(is_phylogeny(ape::rmtree(N = 2, n = 10)))
  testthat::expect_false(is_phylogeny(42))
  testthat::expect_false(is_phylogeny(c(1, 2, 3)))
  testthat::expect_false(is_phylogeny(3.14))
  testthat::expect_false(is_phylogeny("Hello"))

})
