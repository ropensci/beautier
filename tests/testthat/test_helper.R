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

test_that("convert_fasta_file_to_sequences", {
  expect_silent(
    sequences_table <- convert_fasta_file_to_sequences(
      fasta_filename = get_input_fasta_filename()
    )
  )
})

test_that("create_random_fasta", {
  filename <- tempfile()
  expect_silent(
    sequences_table <- create_random_fasta(
      n_taxa = 5,
      sequence_length = 20,
      filename = filename
    )
  )
  expect_equal(file.exists(filename), TRUE)
  has_removed <- file.remove(filename)
  expect_equal(file.exists(filename), FALSE)
})

test_that("save_text", {
  filename <- tempfile()
  text <- c("Hello", "world")
  save_text(
    filename = filename,
    text = text
  )
  expect_equal(file.exists(filename), TRUE)

  # Remove temporary file
  has_removed <- file.remove(filename)
  expect_equal(file.exists(filename), FALSE)
})
