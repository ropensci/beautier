context("initialize_tree_priors")

test_that("use", {


  tree_priors_before <- list(
    create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = NA)
    )
  )

  testit::assert(!are_initialized_tree_priors(tree_priors_before))
  testit::assert(!is_initialized_tree_prior(tree_priors_before[[1]]))

  tree_priors_after <- initialize_tree_priors(tree_priors_before)

  testit::assert(is_yule_tree_prior(tree_priors_after[[1]]))
  testit::assert(is_initialized_yule_tree_prior(tree_priors_after[[1]]))
  testit::assert(is_initialized_tree_prior(tree_priors_after[[1]]))

  testit::assert(are_initialized_tree_priors(tree_priors_after))
})

test_that("re-initialize", {

  tree_priors_before <- list(
    create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1)
    )
  )
  testit::assert(are_initialized_tree_priors(tree_priors_before))
  tree_priors_after <- initialize_tree_priors(tree_priors = tree_priors_before)
  testit::assert(are_initialized_tree_priors(tree_priors_after))

})
