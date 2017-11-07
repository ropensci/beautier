context("get_yule_birth_rate_distr")

test_that("use", {

  testthat::expect_true(
    is_uniform_distribution(
      get_yule_birth_rate_distr(
        create_yule_tree_prior()
      )
    )
  )

})
