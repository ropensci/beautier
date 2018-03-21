context("fasta_to_phylo")

test_that("example", {

  fasta_filename <- beautier::get_fasta_filename()
  crown_age <- 25.0
  phylos <- beautier::fasta_to_phylo(fasta_filename, crown_age = crown_age)
  testthat::expect_equal(5, length(phylos$tip.label))
  testthat::expect_equal(crown_age,
    beautier:::get_phylo_crown_age(phylos)
  )
})

test_that("abuse", {

  testthat::expect_error(
    beautier::fasta_to_phylo(fasta_filename = "absent", crown_age = 15)
  )

  testthat::expect_error(
    beautier::fasta_to_phylo(
      fasta_filename = beautier::get_fasta_filename(),
      crown_age = -42)
  )
})
