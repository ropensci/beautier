context("get_phylogeny_crown_age")

test_that("get_phylogeny_crown_age: basic use", {
   age <- 15
   set.seed(42)
   phylogeny <- PBD::pbd_sim(
     c(0.2, 1, 0.2, 0.0, 0.0), age
   )$tree
   n_taxa <- length(phylogeny$tip.label)
   testit::assert(n_taxa > 0)
   crown_age <- get_phylogeny_crown_age(phylogeny = phylogeny)
   expect_true(all.equal(age, crown_age, tolerance = 0.001))
})

test_that("get_phylogeny_crown_age: abuse", {
    expect_error(get_phylogeny_crown_age(
    phylogeny = c(1, 2, 3)),
    "get_phylogeny_crown_age: phylogeny must be of class 'phylo'"
  )
})
