context("test-check_phylogeny")

test_that("use", {
  phylogeny <- ape::read.tree(text = "(A:1, B:1):1;")
  expect_silent(check_phylogeny(phylogeny))

  # Must stop on non-phylogenies
  expect_error(check_phylogeny("nonsense"))
  expect_error(check_phylogeny(NULL))
  expect_error(check_phylogeny(NA))
  expect_error(check_phylogeny(1))
  expect_error(check_phylogeny(c()))
  expect_error(check_phylogeny(c(1, 2)))
})
