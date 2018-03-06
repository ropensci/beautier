context("create_mrca_prior")

test_that("use", {

  mrca_prior <- create_mrca_prior(
    taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
    mrca_distr = create_normal_distr()
  )

  testthat::expect_true(is_mrca_prior(mrca_prior))
})

test_that("abuse", {

  testthat::expect_error(
    create_mrca_prior(
      taxa_names = NULL,
      mrca_distr = create_normal_distr()
    ),
    "'taxa_names' must a character vector"
  )

  testthat::expect_error(
    create_mrca_prior(
      taxa_names = c("a", "b"),
      mrca_distr = "nonsense"
    ),
    "'mrca_distr' must a distribution, as created by 'create_distr'"
  )

})
