context("is_mrca_prior")

test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- create_mrca_prior(
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
