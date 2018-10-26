context("test-is_init_mrca_prior")


test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  g <- create_mrca_prior(
    name = "prior_name",
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_one_div_x_distr(id = 123),
    clock_prior_distr_id = 42
  )

  expect_true(is_init_mrca_prior(g))
  expect_true(is_init_mrca_prior(NA))
  expect_false(is_init_mrca_prior("nonsense"))
  expect_false(is_init_mrca_prior(NULL))
})


test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  g <- create_mrca_prior(
    name = "prior_name",
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_one_div_x_distr(id = 123),
    clock_prior_distr_id = 42
  )

  testit::assert(is_init_mrca_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_init_mrca_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- NA
  expect_false(is_init_mrca_prior(h))

  # No 'clock_prior_distr_id'
  h <- g[names(g) != "clock_prior_distr_id"]
  expect_false(is_init_mrca_prior(h))

  # Invalid 'clock_prior_distr_id'
  h <- g
  h$clock_prior_distr_id <- NA
  expect_false(is_init_mrca_prior(h))

  # No 'mrca_distr'
  h <- g[names(g) != "mrca_distr"]
  expect_false(is_init_mrca_prior(h))

  # 'mrca_distr' may be NA
  h <- g
  h$mrca_distr <- NA
  expect_true(is_init_mrca_prior(h))

  # Uninitialized 'mrca_distr'
  h <- g
  h$mrca_distr <- create_uniform_distr(id = NA)
  expect_false(is_init_mrca_prior(h))

})
