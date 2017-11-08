context("create_beast2_input_distribution")

test_that("use", {

  testthat::expect_silent(
    beautier::create_beast2_input_distribution(
      ids = "test_output_0",
      tree_priors = create_yule_tree_prior(
        birth_rate_distribution = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_silent(
    beautier::create_beast2_input_distribution(
      ids = "test_output_0",
      clock_models = create_strict_clock_model(),
      tree_priors = create_yule_tree_prior(
        birth_rate_distribution = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_silent(
    beautier::create_beast2_input_distribution(
      ids = "test_output_0",
      tree_priors = create_yule_tree_prior(
        birth_rate_distribution = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_silent(
    beautier::create_beast2_input_distribution(
      ids = "test_output_0",
      site_models = create_jc69_site_model(),
      tree_priors = create_yule_tree_prior(
        birth_rate_distribution = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_silent(
    beautier::create_beast2_input_distribution(
      ids = c("a", "b"),
      tree_priors = list(
        create_yule_tree_prior(
          birth_rate_distribution = create_uniform_distr(id = 1)
        ),
        create_yule_tree_prior(
          birth_rate_distribution = create_uniform_distr(id = 2)
        )
      )
    )
  )

})

test_that("abuse", {

  # Two IDs, one site model
  testthat::expect_error(
    beautier::create_beast2_input_distribution(
      ids = c("a", "b"),
      site_models = create_jc69_site_model()
    )
  )

  # Two IDs, one clock model
  testthat::expect_error(
    beautier::create_beast2_input_distribution(
      ids = c("a", "b"),
      clock_models = create_strict_clock_model()
    )
  )

  # Two IDs, one tree prior
  testthat::expect_error(
    beautier::create_beast2_input_distribution(
      ids = c("a", "b"),
      clock_models = create_yule_tree_prior()
    )
  )

})
