context("create_beast2_input_distr")

test_that("use with one ID", {

  ids <- "test_output_0"

  testthat::expect_silent(
    beautier:::create_beast2_input_distr(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = beautier:::init_clock_models(
        create_strict_clock_models(ids = NA),
        ids = ids
      ),
      tree_priors = beautier:::init_tree_priors(
        create_yule_tree_priors(ids = ids),
        ids = ids,
        distr_id = 1
      )
    )
  )
})

test_that("use with one ID", {

  ids <- c("a", "b")
  site_models <- list(
    create_jc69_site_model(ids[1]),
    create_jc69_site_model(ids[2])
  )
  clock_models <- create_strict_clock_models(ids = ids)
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
