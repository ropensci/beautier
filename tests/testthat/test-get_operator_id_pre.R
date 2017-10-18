context("get_operator_id_pre")

test_that("multiplication works", {

  testthat::expect_equal(
    get_operator_id_pre(
      tree_priors = create_tree_prior(name = "birth_death")
    ),
    "BirthDeath"
  )

  testthat::expect_equal(
    get_operator_id_pre(
      tree_priors = create_tree_prior(name = "coalescent_constant_population")
    ),
    "CoalescentConstant"
  )

  testthat::expect_error(
    get_operator_id_pre(
      tree_priors = "nonsense"
    )
  )

})
