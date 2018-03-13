context("are_mrca_taxa_non_intersecting")

test_that("use", {

  fasta_filename <- get_beautier_path("test_output_5.fas")
  all_taxa_names <- get_taxa_names(fasta_filename)

  prior_one_two <-create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[1:2]
  )
  prior_two_three <-create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[2:3]
  )
  prior_three_four <-create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = all_taxa_names[3:4]
  )
  intersecting_priors <- list(prior_one_two, prior_two_three)
  non_intersecting_priors <- list(prior_one_two, prior_three_four)

  testthat::expect_true(
    beautier:::are_mrca_taxa_non_intersecting(
      non_intersecting_priors
    )
  )

  skip("WIP")
  testthat::expect_false(
    beautier:::are_mrca_taxa_non_intersecting(
      intersecting_priors
    )
  )

})
