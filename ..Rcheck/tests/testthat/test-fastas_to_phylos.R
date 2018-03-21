context("fastas_to_phylos")

test_that("use, one FASTA filename", {

  fasta_filename <- beautier::get_fasta_filename()
  crown_age <- 25.0
  phylos <- beautier::fastas_to_phylos(
    fasta_filenames = fasta_filename,
    crown_age = crown_age
  )
  testthat::expect_equal(length(phylos), 1)
})

test_that("use, two FASTA filenames", {

  fasta_filenames <- get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
  crown_age <- 36.0
  phylos <- beautier::fastas_to_phylos(
    fasta_filenames = fasta_filenames,
    crown_age = crown_age
  )
  testthat::expect_equal(length(phylos), 2)
})

test_that("abuse", {

  testthat::expect_error(
    beautier::fastas_to_phylos(fasta_filenames = "absent", crown_age = 15),
    "'fasta_filenames' must be the names of existing files"
  )

  testthat::expect_error(
    beautier::fastas_to_phylos(
      fasta_filenames = beautier::get_fasta_filename(),
      crown_age = -42
    ),
    "'crown_age' must be nonzero and positive"
  )

})
