context("test-check_mrca_prior")

test_that("use, default", {

  mrca_prior <- create_mrca_prior()
  expect_silent(check_mrca_prior(mrca_prior))

})

test_that("use, alignment ID and taxon names", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
    taxa_names = get_taxa_names(filename = fasta_filename)
  )
  expect_silent(check_mrca_prior(mrca_prior))
})

test_that("negatives", {

  # NA is a valid null-MRCA-prior
  expect_silent(check_mrca_prior(NA))
  expect_error(check_mrca_prior("nonsense"))
  expect_error(check_mrca_prior(NULL))
  expect_error(check_mrca_prior(314))
})

context("test-check_mrca_prior")

test_that("use", {
  expect_silent(check_mrca_prior(create_mrca_prior()))

  expect_error(
    check_mrca_prior(
      create_mrca_prior(alignment_id = c(1, 2))
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(alignment_id = NULL)
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(taxa_names = c(1, 2))
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(taxa_names = NULL)
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(is_monophyletic = "nonsense")
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(is_monophyletic = NULL)
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(mrca_distr = "nonsense")
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(name = c(1, 2))
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(name = NULL)
    )
  )

  skip("Issue 81, #81")
  expect_error(
    check_mrca_prior(
      create_mrca_prior(clock_prior_distr_id = "nonsense")
    )
  )
  expect_error(
    check_mrca_prior(
      create_mrca_prior(clock_prior_distr_id = NULL)
    )
  )

})

test_that("in-depth use", {

  good_mrca_prior <- create_mrca_prior()

  # OK
  expect_silent(
    check_mrca_prior(
      good_mrca_prior
    )
  )

  # Wrong parameter names
  mrca_prior <- good_mrca_prior
  mrca_prior$name <- NULL
  expect_error(
    check_mrca_prior(
      mrca_prior
    ),
    "'name' must be an element of an 'mrca_prior'"
  )

  mrca_prior <- good_mrca_prior
  mrca_prior$alignment_id <- NULL
  expect_error(
    check_mrca_prior(
      mrca_prior
    ),
    "'alignment_id' must be an element of an 'mrca_prior'"
  )

  mrca_prior <- good_mrca_prior
  mrca_prior$taxa_names <- NULL
  expect_error(
    check_mrca_prior(mrca_prior),
    "'taxa_names' must be an element of an 'mrca_prior'"
  )

  mrca_prior <- good_mrca_prior
  mrca_prior$is_monophyletic <- NULL
  expect_error(
    check_mrca_prior(mrca_prior),
    "'is_monophyletic' must be an element of an 'mrca_prior'"
  )

  mrca_prior <- good_mrca_prior
  mrca_prior$mrca_distr <- NULL
  expect_error(
    check_mrca_prior(mrca_prior),
    "'mrca_distr' must be an element of an 'mrca_prior'"
  )

  mrca_prior <- good_mrca_prior
  mrca_prior$clock_prior_distr_id <- NULL
  expect_error(
    check_mrca_prior(mrca_prior),
    "'clock_prior_distr_id' must be an element of an 'mrca_prior'"
  )
})
