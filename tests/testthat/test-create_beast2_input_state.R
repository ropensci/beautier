context("create_beast2_input_state")

test_that("birth_death", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(fasta_filename)
  testthat::expect_silent(
    create_beast2_input_state(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = init_clock_models(
        clock_models = list(create_strict_clock_model()),
        fasta_filenames = fasta_filename,
        distr_id = 0
      ),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()), ids = id, distr_id = 1
      )
    )
  )

  testthat::expect_silent(
    create_beast2_input_state(
      site_models = list(create_gtr_site_model(id = id)),
      clock_models = init_clock_models(
        clock_models = list(create_strict_clock_model()),
        fasta_filenames = fasta_filename,
        distr_id = 0
      ),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()), ids = id, distr_id = 1
      )
    )
  )
})


test_that("use without initial phylogeny", {

  id <- "test_output_0"
  testthat::expect_silent(
    create_beast2_input_state(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model(id = id)),
      tree_priors = init_tree_priors(
        list(create_yule_tree_prior()),
        ids = id
      )
    )
  )

})
