context("create_beast2_input_data")

test_that("sequences are sorted, as per v2.4", {

  lines <- create_beast2_input_data(
      input_fasta_filenames = beastscriptr::get_input_fasta_filename()
    )
  expected <- paste0("                    <sequence id=\"seq_t1\" ",
    "taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>")
  created <- lines[4]
  testthat::expect_equal(expected, created)

})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_data(
      input_fasta_filenames = "abs.ent"
    )
  )
})

test_that("two alignments", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beastscriptr")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beastscriptr")

  testthat::expect_silent(
    create_beast2_input_data(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2)
    )
  )
})
