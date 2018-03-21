context("is_mrca_prior")

test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- create_mrca_prior(
    name = "my_mrca_prior_name",
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )

  testthat::expect_true(is_mrca_prior(mrca_prior))
  testthat::expect_true(is_mrca_prior(NA))

  testthat::expect_false(is_mrca_prior("nonsense"))
  testthat::expect_false(is_mrca_prior(42))
  testthat::expect_false(is_mrca_prior(NULL))
})

test_that("use, two", {

  mrca_prior <- create_mrca_prior(
    name = "my_mrca_prior_name",
    alignment_id = "anthus_aco",
    taxa_names = c("a", "b"),
    mrca_distr = create_one_div_x_distr()
  )
  mrca_priors <- list(mrca_prior, mrca_prior)

  testthat::expect_false(is_mrca_prior(mrca_priors))
  testthat::expect_true(is_mrca_prior(mrca_priors[[1]]))
  testthat::expect_true(is_mrca_prior(mrca_priors[[2]]))

})
