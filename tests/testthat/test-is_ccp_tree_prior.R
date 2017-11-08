context("is_ccp_tree_prior")

test_that("usage", {
  testthat::expect_true(
    is_ccp_tree_prior(
      create_tree_prior(name = "coalescent_constant_population")
    )
  )

  testthat::expect_false(
    is_ccp_tree_prior(
      create_bd_tree_prior()
    )
  )

  testthat::expect_false(
    is_ccp_tree_prior(
      "nonsense"
    )
  )
})
