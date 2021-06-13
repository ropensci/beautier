test_that("use", {

  expect_false(is_mrca_prior_with_distr("nonsense"))
  expect_false(is_mrca_prior_with_distr(NA))

  fasta_filename <- get_beautier_path("anthus_aco.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename)
  )
  expect_false(is_mrca_prior_with_distr(mrca_prior))

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_one_div_x_distr()
  )
  expect_true(is_mrca_prior_with_distr(mrca_prior))
})
