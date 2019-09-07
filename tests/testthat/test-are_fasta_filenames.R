test_that("use", {

  expect_true(are_fasta_filenames("1.fas"))
  expect_true(are_fasta_filenames("1.fasta"))
  expect_true(are_fasta_filenames("1.FAS"))
  expect_true(are_fasta_filenames("1.FASTA"))
  expect_true(are_fasta_filenames(c("1.fas", "2.fas")))

  expect_false(are_fasta_filenames(""))
  expect_false(are_fasta_filenames(NA))
  expect_false(are_fasta_filenames(NULL))
  expect_false(are_fasta_filenames(Inf))
  expect_false(are_fasta_filenames("1.fasX"))

  # One non-FASTA returns a FALSE
  expect_false(are_fasta_filenames(c("1.fas", "2.exe")))
  expect_false(are_fasta_filenames(c("1.bat", "2.exe")))

})
