context("get_taxa_names")

test_that("use", {

  created <- get_taxa_names(get_beautier_path("anthus_aco_sub.fas"))
  expected <- c(
    "61430_aco", "626029_aco", "630116_aco", "630210_aco", "B25702_aco"
  )
  testthat::expect_equal(created, expected)
})
