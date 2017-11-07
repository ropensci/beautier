context("get_operator_id_pre")

test_that("use", {

  testthat::expect_equal(
    get_operator_id_pre(
      tree_prior = create_tree_prior(name = "birth_death")
    ),
    "BirthDeath"
  )

  testthat::expect_equal(
    get_operator_id_pre(
      tree_prior = create_bd_tree_prior()
    ),
    "BirthDeath"
  )

  testthat::expect_equal(
    get_operator_id_pre(
      tree_prior = create_yule_tree_prior()
    ),
    "YuleModel"
  )

  testthat::expect_equal(
    get_operator_id_pre(
      tree_prior = create_yule_tree_priors(n = 1)
    ),
    "YuleModel"
  )

  testthat::expect_equal(
    get_operator_id_pre(
      tree_prior = create_tree_prior(name = "coalescent_constant_population")
    ),
    "CoalescentConstant"
  )

  testthat::expect_error(
    get_operator_id_pre(
      tree_prior = "nonsense"
    )
  )

})
