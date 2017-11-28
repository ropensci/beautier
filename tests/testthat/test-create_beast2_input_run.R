context("create_beast2_input_run")

test_that("usage", {

  id <- "test_output_0"
  testthat::expect_silent(
    beautier:::create_beast2_input_run(
      ids = id,
      tree_priors = list(
        create_yule_tree_prior(
          id = id,
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
