context("get_crown_age")

test_that("get_crown_age: basic use", {

   phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
   created <- get_crown_age(phylogeny = phylogeny)
   expect_true(all.equal(created, 15, tolerance = 0.001))
})

test_that("get_crown_age: abuse", {

  expect_error(
    get_crown_age(
      phylogeny = c(1, 2, 3)
    ),
    "phylogeny must be of class 'phylo'"
  )

  expect_error(
    get_crown_age(
     phylogeny = ape::read.tree(text = "(a:1,b:2):3;")
    ),
    "'phylogeny' must be ultrametric"
  )
})
