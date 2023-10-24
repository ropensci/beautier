context("test-are_init_mrca_priors")

test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  g <- create_mrca_prior(
    name = "prior_name",
    mrca_distr = create_one_div_x_distr(id = 123),
    clock_prior_distr_id = 42
  )
  expect_true(is_init_mrca_prior(g))
  expect_true(are_init_mrca_priors(list(g)))

  expect_false(are_init_mrca_priors("nonsense"))
  expect_false(are_init_mrca_priors(NULL))
})
