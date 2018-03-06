context("create_beast2_input_data_sequences")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_data_sequences(
      get_fasta_filename(),
      nucleotides_uppercase = FALSE
    )
  )

  testthat::expect_silent(
    create_beast2_input_data_sequences(
      get_fasta_filename(),
      nucleotides_uppercase = TRUE
    )
  )

})

test_that("sequences are sorted", {

  lines <- create_beast2_input_data_sequences(
      input_fasta_filename = beautier::get_fasta_filename()
    )
  expected <- paste0("                    <sequence id=\"seq_t1\" ",
    "taxon=\"t1\" ",
    "totalcount=\"4\" value=\"acttgttgcgactgcgcctg\"/>")
  created <- lines[1]
  testthat::expect_equal(expected, created)
})


test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_data_sequences("nonse.nse")
  )
})


test_that("sort order", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_nd2.fas")

  lines <- create_beast2_input_data_sequences(fasta_filename_1)
  testthat::expect_true(substr(lines[11], 35, 46) != "seq_bas3_nd2")
  testthat::expect_true(substr(lines[11], 35, 47) == "seq_FALK1_nd2")

})
