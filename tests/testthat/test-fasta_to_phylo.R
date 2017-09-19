context("fasta_to_phylo")

test_that("example", {

  fasta_filename <- beastscriptr::get_input_fasta_filename()
  phylo <- beastscriptr::fasta_to_phylo(fasta_filename, crown_age = 15)
  testthat::expect_equal(5, length(phylo$tip.label))
  if (1 == 2) {
    testthat::expect_equal(crown_age, wiritttes::get_phylogeny_crown_age(phylo))
  }
})
