context("test-are_initial_phylogenies")

test_that("are_initial_phylogenies", {
  phylogeny <- ape::read.tree(text = "(A:1,B:1);")
  expect_false(are_initial_phylogenies(phylogeny))
  expect_true(are_initial_phylogenies(NA))
  expect_true(are_initial_phylogenies(c(phylogeny)))
  expect_true(are_initial_phylogenies(c(phylogeny, phylogeny)))
})
