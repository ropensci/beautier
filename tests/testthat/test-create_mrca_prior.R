test_that("use, minimal", {

  mrca_prior <- create_mrca_prior()

  expect_true(is_mrca_prior(mrca_prior))

})

test_that("use, taxa names", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    taxa_names = get_taxa_names(fasta_filename)
  )

  expect_true(is_mrca_prior(mrca_prior))
})

test_that("use, ID and taxa names", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename)
  )

  expect_true(is_mrca_prior(mrca_prior))
})

test_that("use, taxa names, MRCA distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )

  expect_true(is_mrca_prior(mrca_prior))
})

test_that("use, ID, taxa names, MRCA distr", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr()
  )
  expect_true(is_mrca_prior(mrca_prior))
})

test_that("use, ID, taxa names, MRCA distr, monophyletic", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename),
    taxa_names = get_taxa_names(fasta_filename),
    mrca_distr = create_normal_distr(),
    is_monophyletic = TRUE
  )

  expect_true(is_mrca_prior(mrca_prior))
})

test_that("abuse", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  expect_error(
    create_mrca_prior(
      name = NULL,
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_normal_distr()
    ),
    "'name' must be one NA or one character string"
  )

  # Checked in more detail by 'check_alignment_id'
  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = NULL,
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_normal_distr()
    )
  )

  # Checked in more detail by 'check_mrca_prior_taxa_names'
  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = NULL,
      mrca_distr = create_normal_distr()
    ),
    "'taxa_names'.*NA.*name"
  )

  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = NULL,
      mrca_distr = create_normal_distr()
    ),
    "'is_monophyletic' must be either TRUE or FALSE"
  )

  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = "nonsense"
    ),
    "'mrca_distr' must a distribution, as created by 'create_distr'"
  )

  # Checked in more detail by 'check_mrca_prior_taxa_names'
  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = c(
        get_taxa_names(fasta_filename),
        get_taxa_names(fasta_filename)[1]
      )
    ),
    "'taxa_names' must be NA or all names must be unique"
  )

  expect_error(
    create_mrca_prior(
      name = "my_prior_name",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = ""
    ),
    "'taxa_names' must be NA or have at least one taxon name"
  )
})
