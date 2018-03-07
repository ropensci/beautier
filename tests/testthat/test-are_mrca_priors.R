context("are_mrca_priors")

test_that("use", {

  mrca_prior <- create_mrca_prior(
    name = "my_prior_name",
    alignment_id = "anthus_aco",
    taxa_names = c("a", "b"),
    mrca_distr = create_one_div_x_distr()
  )
  mrca_priors <- list(mrca_prior, mrca_prior)

  testthat::expect_true(beautier:::are_mrca_priors(mrca_prior))
  testthat::expect_true(beautier:::are_mrca_priors(mrca_priors))

  # Also NA is allowed
  testthat::expect_true(beautier:::are_mrca_priors(NA))

  testthat::expect_false(beautier:::are_mrca_priors("nonsense"))
  testthat::expect_false(beautier:::are_mrca_priors(rep("nonsense", 2)))

  testthat::expect_false(beautier:::are_mrca_priors(NULL))

})
