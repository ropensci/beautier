test_that("use", {
  expect_true(
    files_exist(
      c(get_fasta_filename(), get_fasta_filename())
    )
  )
  expect_false(
    files_exist(
      c(get_fasta_filename(), tempfile())
    )
  )
})
