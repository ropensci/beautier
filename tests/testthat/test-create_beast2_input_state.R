context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogenies = NA
    )
  )
})

test_that("abuse", {

  ids <- c("a", "b")

  # Two ids, one site model
  testthat::expect_error(
    create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_model()
    )
  )

  # Two ids, one clock model
  testthat::expect_error(
    create_beast2_input_state(
      ids = ids,
      clock_models = create_strict_clock_model()
    )
  )

  # Two ids, one tree prior
  testthat::expect_error(
    create_beast2_input_state(
      ids = ids,
      tree_priors = create_yule_tree_prior()
    )
  )

  # Two ids, one phylogeny
  testthat::expect_error(
    create_beast2_input_state(
      ids = ids,
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})

test_that("use without initial phylogeny", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0"
    )
  )

})

test_that("use one with initial phylogeny", {

  phylo <- fasta_to_phylo(get_input_fasta_filename(), crown_age = 42)
  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogenies = phylo
    )
  )

})

test_that("two phylogenies", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = c("Anthus_nd2", "Anthus_aco")
    )
  )


})

test_that("two alignments, two initial phylogenies", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  ids <- get_ids(c(fasta_filename_1, fasta_filename_2))
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 314)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 42)
  initial_phylogenies <- c(phylo1, phylo2)

  testthat::expect_silent(
    create_beast2_input_state(
      ids = ids,
      initial_phylogenies = initial_phylogenies
    )
  )


})
