context("get_yule_birth_rate_distribution")

test_that("use", {

  testthat::expect_true(
    is_uniform_distribution(
      get_yule_birth_rate_distribution(
        create_yule_tree_prior()
      )
    )
  )

})
