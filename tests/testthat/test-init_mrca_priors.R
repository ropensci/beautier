context("init_mrca_priors")

test_that("initialize, without distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  before <- list(
    create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename)
    )
  )
  testit::assert(!beautier:::are_init_mrca_priors(before))
  testit::assert(beautier:::are_mrca_priors(before))
  after <- beautier:::init_mrca_priors(before)
  testit::assert(beautier:::are_init_mrca_priors(after))
  testit::assert(beautier:::are_mrca_priors(after))
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
  testit::assert(!beautier:::are_init_mrca_priors(before))
  testit::assert(beautier:::are_mrca_priors(before))
  after <- beautier:::init_mrca_priors(before)
  testit::assert(beautier:::is_init_mrca_prior(x = after[[1]]))
  testit::assert(beautier:::are_init_mrca_priors(after))
  testit::assert(beautier:::are_mrca_priors(after))
})
