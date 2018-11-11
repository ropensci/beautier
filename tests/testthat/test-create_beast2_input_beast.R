context("create_beast2_input_beast")

test_that("use, one alignment", {

  fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")
  id <- beautier:::get_alignment_id(fasta_filename)

  testthat::expect_silent(
    beautier:::create_beast2_input_beast(
      input_filenames = fasta_filename,
      tree_priors = list(
        create_yule_tree_prior(
          id = id,
          birth_rate_distr = create_uniform_distr(id = 1)
        )
      ),
      clock_models = list(
        create_strict_clock_model(
          id = id,
          clock_rate_distr = create_uniform_distr(id = 2),
          clock_rate_param = create_clock_rate_param(id = 3)
        )
      )
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_beast(input_filenames = "nonsense")
  )

  fasta_filename_1 <- beautier::get_beautier_path("anthus_nd2.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_aco.fas")

  # Two filenames, one site model
  testthat::expect_error(
    create_beast2_input_beast(
      input_filenames = c(fasta_filename_1, fasta_filename_2),
      site_models = create_jc69_site_model()
    )
  )

  # Two filenames, one clock model
  testthat::expect_error(
    create_beast2_input_beast(
      input_filenames = c(fasta_filename_1, fasta_filename_2),
      clock_models = create_strict_clock_model()
    )
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_beast(
      input_filenames = c(fasta_filename_1, fasta_filename_2),
      tree_priors = create_yule_tree_prior()
    )
  )

  # Two filenames, one phylogeny
  testthat::expect_error(
    create_beast2_input_beast(
      input_filenames = c(fasta_filename_1, fasta_filename_2),
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})
