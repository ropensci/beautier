context("is_ccp_tree_prior")

test_that("usage", {
  testthat::expect_true(
    is_ccp_tree_prior(
      create_ccp_tree_prior()
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
