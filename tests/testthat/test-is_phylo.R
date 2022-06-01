test_that("is_phylo: use", {
  check_empty_beautier_folder()

  expect_true(is_phylo(ape::rcoal(3)))
  expect_false(is_phylo(ape::rmtree(N = 2, n = 10)))
  expect_false(is_phylo(42))
  expect_false(is_phylo(c(1, 2, 3)))
  expect_false(is_phylo(3.14))
  expect_false(is_phylo("Hello"))
  expect_false(is_phylo(is_phylo))

  check_empty_beautier_folder()
})
