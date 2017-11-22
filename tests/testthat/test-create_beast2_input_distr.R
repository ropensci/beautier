context("create_beast2_input_distr")

test_that("use with one ID", {

  ids <- "test_output_0"

  testthat::expect_silent(
    beautier:::create_beast2_input_distr(
      ids = ids,
      site_models = create_jc69_site_models(n = 1),
      clock_models = beautier:::init_clock_models(
        create_strict_clock_models(ids = NA), ids = ids),
      tree_priors = beautier:::init_tree_priors(
        create_yule_tree_priors(n = 1),
        distr_id = 1
      )
    )
  )
})

test_that("use with one ID", {

  site_models <- list(
    create_jc69_site_model(),
    create_jc69_site_model()
  )
  clock_models <- create_strict_clock_models(ids = c("a", "b"))
  tree_priors <- list(
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 2)
    )
  )
  testit::assert(beautier:::are_init_tree_priors(tree_priors))

  testthat::expect_silent(
    beautier:::create_beast2_input_distr(
      ids = c("a", "b"),
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

})

test_that("abuse", {

  # Two IDs, one site model
  testthat::expect_error(
    beautier:::create_beast2_input_distr(
      ids = c("a", "b"),
      site_models = create_jc69_site_model()
    )
  )

  # Two IDs, one clock model
  testthat::expect_error(
    beautier:::create_beast2_input_distr(
      ids = c("a", "b"),
      clock_models = create_strict_clock_model(ids = "a")
    )
  )

  # Two IDs, one tree prior
  testthat::expect_error(
    beautier:::create_beast2_input_distr(
      ids = c("a", "b"),
      clock_models = create_yule_tree_prior()
    )
  )

})
