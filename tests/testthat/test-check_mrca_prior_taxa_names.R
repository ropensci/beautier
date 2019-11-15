test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  expect_silent(
    check_mrca_prior_taxa_names(
      taxa_names = NA
    )
  )

  expect_silent(
    check_mrca_prior_taxa_names(
      taxa_names = get_taxa_names(fasta_filename)
    )
  )

  expect_error(
    check_mrca_prior_taxa_names(
      taxa_names = character()
    ),
    "'taxa_names' must be NA or have at least one taxon name"
  )

  expect_error(
    check_mrca_prior_taxa_names(
      taxa_names = ""
    ),
    "'taxa_names' must be NA or have at least one taxon name"
  )
  expect_error(
    check_mrca_prior_taxa_names(
      taxa_names = Inf
    ),
    "'taxa_names' must be NA or have at least one taxon name"
  )

  expect_error(
    check_mrca_prior_taxa_names(
      taxa_names = c(
        get_taxa_names(fasta_filename),
        get_taxa_names(fasta_filename)[1]
      )
    ),
    "'taxa_names' must be NA or all names must be unique"
  )

})
