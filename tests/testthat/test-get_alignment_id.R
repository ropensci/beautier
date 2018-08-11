context("get_alignment_id")

test_that("use", {

  created <- get_alignment_id(beautier::get_beautier_path("anthus_aco_sub.fas"))
  expected <- "anthus_aco_sub"
  testthat::expect_equal(created, expected)

})


test_that("use, capitalization", {

  testthat::expect_equal(
    get_alignment_id(
      "anthus_aco.fas",
      capitalize_first_char_id = FALSE
    ),
    "anthus_aco"
  )

  testthat::expect_equal(
    get_alignment_id(
      "anthus_aco.fas",
      capitalize_first_char_id = TRUE
    ),
    "Anthus_aco"
  )

})
