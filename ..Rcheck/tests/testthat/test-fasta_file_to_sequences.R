context("fasta_file_to_sequences")

test_that("fasta_file_to_sequences: use", {
  expect_silent(
    sequences_table <- fasta_file_to_sequences(
      fasta_filename = get_fasta_filename()
    )
  )
})

test_that("fasta_file_to_sequences: abuse", {
  expect_error(
    fasta_file_to_sequences(
      fasta_filename = "nonexist.ing"
    )
  )
})
