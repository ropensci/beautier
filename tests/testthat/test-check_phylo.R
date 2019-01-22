context("test-check_phylo")

test_that("use", {
  phylogeny <- ape::read.tree(text = "((A:1, B:1):1;")
  expect_silent(check_phylo(phylogeny))

  # Must stop on non-phylogenies
  expect_error(check_phylo(phylo = "nonsense"))
  expect_error(check_phylo(phylo = NULL))
  expect_error(check_phylo(phylo = NA))
})
