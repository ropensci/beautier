context("are_init_tree_priors")

test_that("detect nonsense", {

  expect_false(are_init_tree_priors("nonsense"))

})

test_that("detect initialized Yule priors", {

  init_yule_priors <- list(
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expect_true(
    are_init_tree_priors(init_yule_priors)
  )
})


test_that("detect uninitialized Yule priors", {

  uninit_yule_priors <- list(
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = NA)
    )
  )

  expect_false(
    are_init_tree_priors(uninit_yule_priors)
  )

})

test_that("detect initialized BD priors", {

  init_bd_priors <- list(
    create_bd_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1),
      death_rate_distr = create_uniform_distr(id = 2)
    )
  )
  expect_true(
    are_init_tree_priors(init_bd_priors)
  )
})

test_that("detect uninitialized BD priors", {

  uninit_bd_priors <- list(
    create_bd_tree_prior(
      birth_rate_distr = create_uniform_distr(id = NA),
      death_rate_distr = create_uniform_distr(id = NA)
    )
  )
  expect_false(
    are_init_tree_priors(uninit_bd_priors)
  )
})
