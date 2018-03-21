context("is_phylo")

test_that("is_phylo: use", {

  testthat::expect_true(is_phylo(ape::rcoal(3)))
  testthat::expect_false(is_phylo(ape::rmtree(N = 2, n = 10)))
  testthat::expect_false(is_phylo(42))
  testthat::expect_false(is_phylo(c(1, 2, 3)))
  testthat::expect_false(is_phylo(3.14))
  testthat::expect_false(is_phylo("Hello"))

})
