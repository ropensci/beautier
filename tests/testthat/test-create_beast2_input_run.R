context("create_beast2_input_run")

test_that("usage", {

  fasta_filename <- beautier:::get_path("anthus_aco.fas")
  id <- beautier:::get_id(fasta_filename)

  testthat::expect_silent(
    beautier:::create_beast2_input_run(
      ids = id,
      tree_priors = list(
        create_yule_tree_prior(
          id = id,
          birth_rate_distr = create_uniform_distr(id = 1)
        )
      ),
      clock_models = beautier:::init_clock_models(
        fasta_filenames = fasta_filename,
        clock_models = list(create_strict_clock_model())
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
