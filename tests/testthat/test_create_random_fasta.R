context("create_random_fasta")

test_that("checks input", {
  expect_silent(
    create_random_fasta(
      n_taxa = 5,
      sequence_length = 20,
      filename = "test.fasta"
    )
  )

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
      filename = "" # Error
    )
  )
})
