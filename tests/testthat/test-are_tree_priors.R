context("are_tree_priors")

test_that("use", {


  testthat::expect_true(are_tree_priors(
    create_yule_tree_prior(id = "some_id")))

  testthat::expect_true(are_tree_priors(
    create_yule_tree_priors(ids = c("a", "b"))))

  testthat::expect_false(are_tree_priors("nonsense"))
  testthat::expect_false(are_tree_priors(rep("nonsense", 2)))

  testthat::expect_false(are_tree_priors(NA))
  testthat::expect_false(are_tree_priors(NULL))

})
