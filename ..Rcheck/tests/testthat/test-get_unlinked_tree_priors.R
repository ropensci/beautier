context("get_unlinked_tree_priors")

test_that("use", {

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_tree_priors(
        list(
          create_yule_tree_prior(id = "a")
        )
      )
    ),
    1
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_tree_priors(
        list(
          create_yule_tree_prior(id = "a"),
          create_yule_tree_prior(id = "b")
        )
      )
    ),
    2
  )

  testthat::expect_equal(
    length(
      beautier:::get_unlinked_tree_priors(
        list(
          create_yule_tree_prior(id = "a"),
          create_yule_tree_prior(id = "a")
        )
      )
    ),
    1
  )

})
