context("create_mrca_prior")

test_that("use, no MRCA distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename)
  )

  testthat::expect_true(is_mrca_prior(mrca_prior))
})

test_that("use, with MRCA distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )

  testthat::expect_true(is_mrca_prior(mrca_prior))
})

test_that("abuse", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  testthat::expect_error(
    create_mrca_prior(
      name = NULL,
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_normal_distr()
    ),
    "'name' must be NA or characters"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = NULL,
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_normal_distr()
    ),
    "'alignment_id' must be characters"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = NULL,
      mrca_distr = create_normal_distr()
    ),
    "'taxa_names' must a character vector"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = NULL,
      mrca_distr = create_normal_distr()
    ),
    "'is_monophyletic' must be either TRUE or FALSE"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = "nonsense"
    ),
    "'mrca_distr' must a distribution, as created by 'create_distr'"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = c(
        get_taxa_names(fasta_filename),
        get_taxa_names(fasta_filename)[1]
      )
    ),
    "All names of 'taxa_names' must be unique"
  )

  testthat::expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = ""
    ),
    "'taxa_names' must have at least one taxon name"
  )

})
