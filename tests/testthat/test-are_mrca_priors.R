context("are_mrca_priors")

test_that("use", {

  mrca_prior <- create_mrca_prior(
    name = "my_prior_name",
    alignment_id = "anthus_aco",
    taxa_names = c("a", "b"),
    mrca_distr = create_one_div_x_distr()
  )
  expect_true(is_mrca_prior(mrca_prior))
  mrca_priors <- list(mrca_prior, mrca_prior)
  expect_true(are_mrca_priors(mrca_priors))

  # Also NA is allowed
  expect_true(are_mrca_priors(NA))

  expect_false(are_mrca_priors("nonsense"))
  expect_false(are_mrca_priors(rep("nonsense", 2)))

  expect_false(are_mrca_priors(NULL))
})
