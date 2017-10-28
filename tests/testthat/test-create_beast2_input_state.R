context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogenies = NA
    )
  )
})

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0"
    )
  )


})

test_that("two phylogenies", {

  skip("WIP")
  testthat::expect_silent(
    create_beast2_input_state(
      ids = c("Anthus_nd2", "Anthus_aco")
    )
  )

})

