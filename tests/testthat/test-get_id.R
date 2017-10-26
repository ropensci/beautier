context("get_id")

test_that("use", {

  testthat::expect_equal(
    get_id("anthus_aco.fas"),
    "Anthus_aco"
  )

})
