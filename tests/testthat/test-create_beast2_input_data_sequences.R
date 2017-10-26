context("create_beast2_input_data_sequences")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_data_sequences(
      get_input_fasta_filename(),
      nucleotides_uppercase = FALSE
    )
  )

  testthat::expect_silent(
    create_beast2_input_data_sequences(
      get_input_fasta_filename(),
      nucleotides_uppercase = TRUE
    )
  )

})

test_that("sequences are sorted", {

  lines <- create_beast2_input_data_sequences(
      input_fasta_filename = beastscriptr::get_input_fasta_filename()
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
