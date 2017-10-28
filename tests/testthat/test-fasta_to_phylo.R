context("fasta_to_phylo")

test_that("example", {

  fasta_filename <- beastscriptr::get_input_fasta_filename()
  crown_age <- 25.0
  phylos <- beastscriptr::fasta_to_phylo(fasta_filename, crown_age = crown_age)
  testthat::expect_equal(5, length(phylos[[1]]$tip.label))
  testthat::expect_equal(crown_age,
    beastscriptr::get_phylogeny_crown_age(phylos[[1]]))
})

test_that("abuse", {

  testthat::expect_error(
    beastscriptr::fasta_to_phylo(fasta_filename = "absent", crown_age = 15)
  )

  testthat::expect_error(
    beastscriptr::fasta_to_phylo(
      fasta_filename = beastscriptr::get_input_fasta_filename(),
      crown_age = -42)
  )
})
