context("get_alignment_id")

test_that("use", {

  created <- get_alignment_id(beautier::get_beautier_path("anthus_aco_sub.fas"))
  expected <- "anthus_aco_sub"
  testthat::expect_equal(created, expected)

})
