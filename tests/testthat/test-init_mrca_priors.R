test_that("initialize, without distr, v2.4", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  before <- list(
    create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename)
    )
  )
  testthat::expect_true(!are_init_mrca_priors(before))
  testthat::expect_true(are_mrca_priors(before))
  after <- init_mrca_priors(before, beauti_options = create_beauti_options())
  testthat::expect_true(are_init_mrca_priors(after))
  testthat::expect_true(are_mrca_priors(after))
})

test_that("initialize, without distr, v2.6", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  before <- list(
    create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename)
    )
  )
  testthat::expect_true(!are_init_mrca_priors(before))
  testthat::expect_true(are_mrca_priors(before))
  after <- init_mrca_priors(before, beauti_options = create_beauti_options_v2_6())
  testthat::expect_true(are_init_mrca_priors(after))
  testthat::expect_true(are_mrca_priors(after))
  expect_equal(after[[1]]$name, "ingroup")
})

test_that("initialize, with distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  before <- list(
    create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_one_div_x_distr()
    )
  )
  testthat::expect_true(!beautier::are_init_mrca_priors(before))
  testthat::expect_true(beautier::are_mrca_priors(before))
  after <- init_mrca_priors(before, beauti_options = create_beauti_options())
  testthat::expect_true(beautier::is_init_mrca_prior(x = after[[1]]))
  testthat::expect_true(beautier::are_init_mrca_priors(after))
  testthat::expect_true(beautier::are_mrca_priors(after))
})
