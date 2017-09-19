context("fasta_to_phylo")

test_that("example", {

  fasta_filename <- beastscriptr::get_input_fasta_filename()
  crown_age <- 25.0
  phylo <- beastscriptr::fasta_to_phylo(fasta_filename, crown_age = crown_age)
  testthat::expect_equal(5, length(phylo$tip.label))
  testthat::expect_equal(crown_age, wiritttes::get_phylogeny_crown_age(phylo))

})
