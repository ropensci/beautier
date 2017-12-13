context("tree_priors_to_xml_tracelog")

test_that("creates a text", {

  testthat::expect_true(
    is.character(
      beautier:::tree_priors_to_xml_tracelog(
        create_yule_tree_priors(ids = "anthus_aco")
      )
    )
  )

})
