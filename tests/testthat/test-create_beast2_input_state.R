context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    create_beast2_input_state(
      ids = "test_output_0",
      initial_phylogenies = NA
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
    "anthus_aco.fas", package = "beastscriptr")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beastscriptr")
  id1 <- get_id(fasta_filename_1, capitalize_first_char_id = TRUE)
  id2 <- get_id(fasta_filename_2, capitalize_first_char_id = TRUE)
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 314)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 42)
  initial_phylogenies <- c(phylo1, phylo2)

  testthat::expect_silent(
    create_beast2_input_state(
      ids = c(id1, id2),
      initial_phylogenies = initial_phylogenies
    )
  )


})

