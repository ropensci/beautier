context("create_random_fasta")

test_that("create_random_fasta: use", {
  filename <- tempfile()
  testthat::expect_silent(
    sequences_table <- beautier:::create_random_fasta(
      n_taxa = 5,
      sequence_length = 20,
      fasta_filename = filename
    )
  )
  expect_true(file.exists(filename))
  file.remove(filename)
  expect_false(file.exists(filename))
})

test_that("create_random_fasta: abuse", {

  expect_error(
    create_random_fasta(
      n_taxa = 0, #Error
      sequence_length = 20,
      filename = "test.fasta"
    )
  )

  expect_error(
    create_random_fasta(
      n_taxa = 5,
      sequence_length = 0, # Error
      filename = "test.fasta"
    )
  )

  expect_error(
    create_random_fasta(
      n_taxa = 5,
      sequence_length = 20,
      filename = ape::rcoal(4)
    )
  )

  expect_error(
    create_random_fasta(
      n_taxa = 5,
      sequence_length = 20,
      filename = "" # Error
    )
  )
})
