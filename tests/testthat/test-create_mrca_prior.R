context("create_mrca_prior")

test_that("use", {

  mrca_prior <- create_mrca_prior(
    taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas"))
  )

  testthat::expect_true(is_mrca_prior(mrca_prior))
})
