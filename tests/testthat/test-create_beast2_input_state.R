context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogeny = NA
    )
  )
})

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogeny = NA
    )
  )
})
