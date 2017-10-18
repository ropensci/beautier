context("is_beast2_input_file")

test_that("testing FASTA file is not a valid BEAST2 input file", {
  testthat::expect_false(
    is_beast2_input_file(
      beastscriptr::get_input_fasta_filename()
    )
  )
})

test_that("testing BEAST2 input file is a valid BEAST2 input file", {

  testthat::expect_true(
    is_beast2_input_file(
      beastscriptr::get_output_xml_filename()
    )
  )

})

test_that("abuse", {

  testthat::expect_error(
    is_beast2_input_file("abs.ent")
  )

})
