context("rnd_phylogeny_to_xml_init")

test_that("use", {

  testthat::expect_silent(
    rnd_phylogeny_to_xml_init("my_id")
  )

})

test_that("abuse", {

  testthat::expect_error(
    rnd_phylogeny_to_xml_init(ape::roal(3))
  )

})
