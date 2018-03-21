context("get_ids")

test_that("use", {

  testthat::expect_equal(
    get_ids("anthus_aco.fas",
      capitalize_first_char_id = FALSE),
    "anthus_aco"
  )

  testthat::expect_equal(
    get_ids("anthus_aco.fas",
      capitalize_first_char_id = TRUE),
    "Anthus_aco"
  )

  testthat::expect_equal(
    get_ids(c("a.fas", "b.fas")),
    c("a", "b")
  )

})
