context("create_beast2_input_run")

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_run(
      ids = "test_output_0",
      tree_priors = list(
        create_yule_tree_prior(
          birth_rate_distr = create_uniform_distr(id = 1)
        )
      )
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
