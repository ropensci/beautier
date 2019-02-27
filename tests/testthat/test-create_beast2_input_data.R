context("create_beast2_input_data")

test_that("sequences are sorted, as per v2.4", {

  lines <- create_beast2_input_data(
      input_filenames = beautier::get_fasta_filename()
    )
  expected <- paste0("                    <sequence id=\"seq_t1\" ",
    "taxon=\"t1\" totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>") # nolint this is no absolute path
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

test_that("alignment start with a capital", {

  fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")

  lines <- create_beast2_input_data(
    input_filenames = c(fasta_filename),
    create_beauti_options(
      capitalize_first_char_id = TRUE
    )
  )
  testthat::expect_equal(lines[2], "id=\"Anthus_aco\"")
})
