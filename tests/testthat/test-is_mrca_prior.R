context("is_mrca_prior")

test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- create_mrca_prior(
    name = "my_mrca_prior_name",
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )

  expect_true(is_mrca_prior(mrca_prior))
  expect_true(is_mrca_prior(NA))

  expect_false(is_mrca_prior("nonsense"))
  expect_false(is_mrca_prior(42))
  expect_false(is_mrca_prior(NULL))
})

test_that("use, two", {

  mrca_prior <- create_mrca_prior(
    name = "my_mrca_prior_name",
    alignment_id = "anthus_aco",
    taxa_names = c("a", "b"),
    mrca_distr = create_one_div_x_distr()
  )
  mrca_priors <- list(mrca_prior, mrca_prior)

  expect_false(is_mrca_prior(mrca_priors))
  expect_true(is_mrca_prior(mrca_priors[[1]]))
  expect_true(is_mrca_prior(mrca_priors[[2]]))

})

test_that("devious", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  g <- create_mrca_prior(
    name = "my_mrca_prior_name",
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )
  expect_true(beautier:::is_mrca_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(beautier:::is_mrca_prior(h))

  # No 'alignment_id'
  h <- g[names(g) != "alignment_id"]
  expect_false(beautier:::is_mrca_prior(h))

  # No 'taxa_names'
  h <- g[names(g) != "taxa_names"]
  expect_false(beautier:::is_mrca_prior(h))

  # No 'is_monophyletic'
  h <- g[names(g) != "is_monophyletic"]
  expect_false(beautier:::is_mrca_prior(h))

  # No 'mrca_distr'
  h <- g[names(g) != "mrca_distr"]
  expect_false(beautier:::is_mrca_prior(h))

  # No 'clock_prior_distr_id'
  h <- g[names(g) != "clock_prior_distr_id"]
  expect_false(beautier:::is_mrca_prior(h))

  # mrca_distr is not a distribution
  h <- g
  h$mrca_distr <- "nonsense"
  expect_false(beautier:::is_mrca_prior(h))
})
