context("create_beast2_input_run")

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_run(
      ids = "test_output_0"
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_run(
      ids = c("a", "b"),
      initial_phylogenies = c(ape::rcoal(4))
    )
  )
})
