context("helper")

test_that("locate test_output_0.fas", {
  filename <- get_input_fasta_filename()
  expect_equal(file.exists(filename), TRUE)
  expect_equal(file.exists(get_input_fasta_filename()), TRUE)
})

test_that("locate birth_death_0_20151005.xml", {
  filename <- get_output_xml_filename()
  expect_equal(file.exists(filename), TRUE)
  expect_equal(file.exists(get_output_xml_filename()), TRUE)
})

