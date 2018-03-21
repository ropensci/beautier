context("get_id")

test_that("use", {

  testthat::expect_equal(
    get_id("anthus_aco.fas",
      capitalize_first_char_id = FALSE),
    "anthus_aco"
  )

  testthat::expect_equal(
    get_id("anthus_aco.fas",
      capitalize_first_char_id = TRUE),
    "Anthus_aco"
  )

})
