context("create_yule_tree_priors")

test_that("use", {

  testthat::expect_silent(create_yule_tree_priors(ids = "some_id"))

})

test_that("create one", {

  m <- create_yule_tree_priors(ids = "some_id")
  testthat::expect_equal(length(m), 1)
  testthat::expect_true(is_yule_tree_prior(m[[1]]))

})

test_that("create two", {

  m <- create_yule_tree_priors(ids = c("a", "b"))
  testthat::expect_equal(length(m), 2)
  testthat::expect_true(is_yule_tree_prior(m[[1]]))
  testthat::expect_true(is_yule_tree_prior(m[[2]]))

})
