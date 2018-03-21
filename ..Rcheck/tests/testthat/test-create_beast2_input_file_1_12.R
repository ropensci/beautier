context("create_beast2_input_file_1_12")

test_that("use", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  testthat::expect_silent(
    create_beast2_input_file_1_12(
      get_fasta_filename(),
      output_filename
    )
  )

  testthat::expect_true(file.exists(output_filename))

})

test_that("create data set of two alignments, with/out fixed crown ages", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  fasta_filename_1 <- get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- get_beautier_path("anthus_nd2.fas")
  fasta_filenames <- c(fasta_filename_1, fasta_filename_2)

  phylo_1_15 <- fasta_to_phylo(fasta_filename_1, crown_age = 15)
  phylo_2_15 <- fasta_to_phylo(fasta_filename_2, crown_age = 15)
  phylo_2_26 <- fasta_to_phylo(fasta_filename_2, crown_age = 26)

  testthat::expect_silent(
    create_beast2_input_file_1_12(
      fasta_filenames,
      "anthus_na_na.xml",
      mcmc = create_mcmc(chain_length = 10000, store_every = 1000)
    )
  )

  testthat::expect_silent(
    create_beast2_input_file_1_12(
      fasta_filenames,
      "anthus_15_15.xml",
      mcmc = create_mcmc(chain_length = 10000, store_every = 1000),
      fixed_crown_ages = c(TRUE, TRUE),
      initial_phylogenies = list(phylo_1_15, phylo_2_15)
    )
  )

  testthat::expect_silent(
    create_beast2_input_file_1_12(
      fasta_filenames,
      "anthus_na_15.xml",
      mcmc = create_mcmc(chain_length = 10000, store_every = 1000),
      fixed_crown_ages = c(FALSE, TRUE),
      initial_phylogenies = list(NA, phylo_2_15)
    )
  )

    testthat::expect_silent(
    create_beast2_input_file_1_12(
      fasta_filenames,
      "anthus_15_na.xml",
      mcmc = create_mcmc(chain_length = 10000, store_every = 1000),
      fixed_crown_ages = c(TRUE, FALSE),
      initial_phylogenies = list(phylo_1_15, NA)
    )
  )

  testthat::expect_silent(
    create_beast2_input_file_1_12(
      fasta_filenames,
      "anthus_15_26.xml",
      mcmc = create_mcmc(chain_length = 10000, store_every = 1000),
      fixed_crown_ages = c(TRUE, TRUE),
      initial_phylogenies = list(phylo_1_15, phylo_2_26)
    )
  )

})
