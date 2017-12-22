context("create_random_phylogeny")

test_that("use", {

  testthat::expect_silent(
    beautier:::create_random_phylogeny(n_taxa = 4)
  )

  testthat::expect_silent(
    beautier:::create_random_phylogeny(n_taxa = 4, taxon_name_ext = "_aco")
  )
})

test_that("taxon names, no taxon name extension", {

  taxon_names <- beautier:::create_random_phylogeny(n_taxa = 4)$tip.label
  testthat::expect_equal(sort(taxon_names), paste0("t", seq(1, 4)))

})

test_that("taxon names, taxon name extension", {

  ext <- "_aco"
  taxon_names <- beautier:::create_random_phylogeny(
    n_taxa = 4,
    taxon_name_ext = ext
  )$tip.label

  testthat::expect_equal(sort(taxon_names), paste0("t", seq(1, 4), ext))

})
