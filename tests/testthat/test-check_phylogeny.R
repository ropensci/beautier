context("test-check_phylogeny")

test_that("use", {
  phylogeny <- ape::read.tree(text = "(A:1, B:1):1;")
  expect_silent(check_phylogeny(phylogeny))

  # Must stop on non-phylogenies
  expect_error(check_phylogeny(phylo = "nonsense"))
  expect_error(check_phylogeny(phylo = NULL))
  expect_error(check_phylogeny(phylo = NA))
})
