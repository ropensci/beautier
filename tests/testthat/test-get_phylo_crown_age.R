context("get_phylo_crown_age")

test_that("get_phylo_crown_age: basic use", {

   phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
   created <- beautier:::get_phylo_crown_age(phylogeny = phylogeny)
   testthat::expect_true(all.equal(created, 15, tolerance = 0.001))
})

test_that("get_phylo_crown_age: abuse", {

  testthat::expect_error(
    beautier:::get_phylo_crown_age(
      phylogeny = c(1, 2, 3)
    ),
    "phylogeny must be of class 'phylo'"
  )

})
