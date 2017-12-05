context("create_beast2_input_distr")

test_that("use with one ID", {

  fasta_filename <- beautier:::get_path("test_output_0.fas")
  id <- beautier:::get_id(fasta_filename)

  testthat::expect_silent(
    beautier:::create_beast2_input_distr(
      site_models = create_jc69_site_models(ids = id),
      clock_models = beautier:::init_clock_models(
        create_strict_clock_models(ids = NA),
        fasta_filenames = fasta_filename
      ),
      tree_priors = beautier:::init_tree_priors(
        create_yule_tree_priors(ids = id),
        ids = id,
        distr_id = 1
      )
    )
  )
})

test_that("use with one ID", {

  fasta_filename <- beautier:::get_path("test_output_5.fas")
  id <- beautier:::get_id(fasta_filename)

  site_models <- list(
    create_jc69_site_model(id),
    create_jc69_site_model(id)
  )
  clock_models <- create_strict_clock_models(ids = id)
  tree_priors <- list(
    create_yule_tree_prior(
      id = id,
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    create_yule_tree_prior(
      id = id,
      birth_rate_distr = create_uniform_distr(id = 2)
    )
  )
  testit::assert(beautier:::are_init_tree_priors(tree_priors))

  testthat::expect_silent(
    beautier:::create_beast2_input_distr(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

})
