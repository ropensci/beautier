context("create_beast2_input_data")

test_that("sequences are sorted, as per v2.4", {

  lines <- create_beast2_input_data(
      input_filenames = beautier::get_fasta_filename()
    )
  expected <- paste0("                    <sequence id=\"seq_t1\" ",
    "taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>")
  created <- lines[4]
  testthat::expect_equal(expected, created)

})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_data(
      input_filenames = "abs.ent"
    )
  )
})

test_that("two alignments", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_nd2.fas")

  testthat::expect_silent(
    create_beast2_input_data(
      input_filenames = c(fasta_filename_1, fasta_filename_2)
    )
  )
})

test_that("alignments start with a capital", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_nd2.fas")

  lines <- create_beast2_input_data(
    input_filenames = c(fasta_filename_1, fasta_filename_2),
    create_misc_options(
      capitalize_first_char_id = TRUE
    )
  )
  testthat::expect_equal(lines[2], "id=\"Anthus_aco\"")

})
